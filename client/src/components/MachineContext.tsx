import { noop } from 'lodash'
import mitt, { Emitter } from 'mitt'
import {
  MutableRefObject,
  ReactNode,
  createContext,
  forwardRef,
  useCallback,
  useContext,
  useImperativeHandle,
  useMemo,
  useRef,
  useState,
} from 'react'

import type { RigidBody } from '@dimforge/rapier2d'
import { coords } from '../lib/coords'
import { BallSnapshot, BodySnapshot, snapshotBody } from '../lib/snapshot'
import { inBounds, useIdGen } from '../lib/utils'
import { BallType, Bounds } from '../types'
import { PhysicsContext } from './PhysicsContext'

export interface BallData {
  id: string
  snapshot: BodySnapshot
  age: number
  type: BallType
  overrideDamping?: number
}

export type BallDestroyReason = 'expiry'

export type BallFollowCallback = (x: number, y: number) => void

type MachineEvents = {
  createBall: BallData
  destroyBall: string
  exitBall: string
  renewBall: string
  killBall: string
  followBall: BallFollowCallback
  unfollowBall: void
  ballExpired: void
}

type ActiveBalls = Record<
  string,
  {
    type: BallType
    body: RigidBody
    renewTick: number
  }
>

export type CreateBallOptions = {
  vx?: number
  vy?: number
  overrideDamping?: number
}

export type MachineContextType = {
  msPerBall: number
  createBall: (
    x: number,
    y: number,
    type: BallType,
    opts?: CreateBallOptions,
  ) => void
  destroyBall: (id: string, reason?: BallDestroyReason) => void
  /** Destroy the ball when it leaves the viewable area */
  exitBall: (id: string) => void
  renewBall: (id: string) => void
  killBall: (id: string) => void
  clearBalls: () => void
  followBall: (callback: BallFollowCallback) => void
  unfollowBall: () => void
  registerBall: (
    id: string,
    type: BallType,
    body: RigidBody,
    renewTick: number,
  ) => void
  unregisterBall: (id: string) => void
  snapshotBalls: (bounds: Bounds) => BallSnapshot[]
  restoreBalls: (ballSnapshots: BallSnapshot[]) => void
  simulationBoundsRef: MutableRefObject<Bounds>
  viewBoundsRef: MutableRefObject<Bounds>
  events: Emitter<MachineEvents>
}

export interface MachineContextProviderRef {
  createBall: MachineContextType['createBall']
  clearBalls: MachineContextType['clearBalls']
  followBall: MachineContextType['followBall']
  unfollowBall: MachineContextType['unfollowBall']
  events: MachineContextType['events']
  simulationBoundsRef: MachineContextType['simulationBoundsRef']
  viewBoundsRef: MachineContextType['viewBoundsRef']
}

const infiniteBounds: Bounds = [-Infinity, -Infinity, Infinity, Infinity]

export const MachineContext = createContext<MachineContextType>({
  msPerBall: Infinity,
  createBall: noop,
  destroyBall: noop,
  exitBall: noop,
  renewBall: noop,
  killBall: noop,
  followBall: noop,
  unfollowBall: noop,
  clearBalls: noop,
  registerBall: noop,
  unregisterBall: noop,
  snapshotBalls: () => [],
  restoreBalls: noop,
  simulationBoundsRef: { current: infiniteBounds },
  viewBoundsRef: { current: infiniteBounds },
  events: mitt(),
})

export const MachineContextProvider = forwardRef(
  function MachineContextProvider(
    {
      msPerBall,
      initialSimulationBounds = infiniteBounds,
      initialViewBounds = infiniteBounds,
      children,
    }: {
      msPerBall: number
      initialSimulationBounds?: Bounds
      initialViewBounds?: Bounds
      children: ReactNode
    },
    ref,
  ) {
    const physics = useContext(PhysicsContext)

    const nextBallId = useIdGen()

    const [events] = useState(() => mitt<MachineEvents>())

    const activeBalls = useRef<ActiveBalls>({})

    const createBall = useCallback(
      (
        x: number,
        y: number,
        type: BallType,
        { vx = 0, vy = 0, overrideDamping }: CreateBallOptions = {},
      ) => {
        events.emit('createBall', {
          id: nextBallId(),
          snapshot: {
            x: coords.toRapier.x(x),
            y: coords.toRapier.y(y),
            angle: 0,
            vx,
            vy,
            va: 0,
          },
          age: 0,
          type,
          overrideDamping,
        })
      },
      [events, nextBallId],
    )

    const destroyBall = useCallback(
      (id: string, reason?: BallDestroyReason) => {
        events.emit('destroyBall', id)
        if (reason === 'expiry') {
          events.emit('ballExpired')
        }
      },
      [events],
    )

    const exitBall = useCallback(
      (id: string) => {
        events.emit('exitBall', id)
      },
      [events],
    )

    const renewBall = useCallback(
      (id: string) => {
        events.emit('renewBall', id)
      },
      [events],
    )

    const killBall = useCallback(
      (id: string) => {
        events.emit('killBall', id)
      },
      [events],
    )

    const followBall = useCallback(
      (callback: BallFollowCallback) => {
        events.emit('followBall', callback)
      },
      [events],
    )

    const unfollowBall = useCallback(() => {
      events.emit('unfollowBall')
    }, [events])

    const clearBalls = useCallback(() => {
      Object.keys(activeBalls.current).forEach((id) => destroyBall(id))
    }, [destroyBall])

    const registerBall = useCallback(
      (id: string, type: BallType, body: RigidBody, renewTick: number) => {
        activeBalls.current[id] = { type, body, renewTick }
      },
      [],
    )

    const unregisterBall = useCallback((id: string) => {
      delete activeBalls.current[id]
    }, [])

    const snapshotBalls = useCallback(
      (bounds: Bounds) => {
        if (!physics) {
          return []
        }
        const currentTick = physics.getCurrentTick()
        return Object.values(activeBalls.current)
          .filter(({ body }) => {
            const { x, y } = body.translation()
            return inBounds(x, y, bounds)
          })
          .map(({ type, body, renewTick }) => ({
            ...snapshotBody(body),
            age: currentTick - renewTick,
            type,
          }))
      },
      [physics],
    )

    const restoreBalls = useCallback(
      (ballSnapshots: BallSnapshot[]) => {
        for (const { type, age, ...snapshot } of ballSnapshots) {
          events.emit('createBall', {
            id: nextBallId(),
            snapshot,
            age,
            type,
          })
        }
      },
      [events, nextBallId],
    )

    const simulationBoundsRef = useRef<Bounds>(initialSimulationBounds)
    const viewBoundsRef = useRef<Bounds>(initialViewBounds)

    const contextValue: MachineContextType = useMemo(
      () => ({
        msPerBall,
        createBall,
        destroyBall,
        exitBall,
        renewBall,
        killBall,
        followBall,
        unfollowBall,
        clearBalls,
        registerBall,
        unregisterBall,
        snapshotBalls,
        restoreBalls,
        simulationBoundsRef,
        viewBoundsRef,
        events,
      }),
      [
        msPerBall,
        createBall,
        destroyBall,
        exitBall,
        renewBall,
        killBall,
        followBall,
        unfollowBall,
        clearBalls,
        registerBall,
        unregisterBall,
        snapshotBalls,
        restoreBalls,
        events,
      ],
    )

    useImperativeHandle(ref, () => ({
      createBall,
      followBall,
      unfollowBall,
      clearBalls,
      events,
      simulationBoundsRef,
      viewBoundsRef,
    }))

    return (
      <MachineContext.Provider value={contextValue}>
        {children}
      </MachineContext.Provider>
    )
  },
)

export function useMachine() {
  return useContext(MachineContext)
}
