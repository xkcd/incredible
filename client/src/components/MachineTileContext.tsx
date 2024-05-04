import { mapValues, noop } from 'lodash'
import {
  DependencyList,
  ReactNode,
  createContext,
  forwardRef,
  useCallback,
  useContext,
  useImperativeHandle,
  useMemo,
  useRef,
} from 'react'

import type RAPIER from '@dimforge/rapier2d'
import type {
  Collider,
  ColliderDesc,
  RigidBody,
  RigidBodyDesc,
} from '@dimforge/rapier2d'
import { coords } from '../lib/coords'
import {
  MachineSnapshot,
  applySnapshotToBody,
  offsetSnapshot,
  snapshotBody,
} from '../lib/snapshot'
import { inBounds } from '../lib/utils'
import { Bounds, isBall } from '../types'
import { useMachine } from './MachineContext'
import { useLoopHandler, useRapierEffect } from './PhysicsContext'

type ActiveBodies = Record<string, RigidBody>

export type MachineTileContextType = {
  bounds: Bounds
  width: number
  height: number
  registerBody: (key: string, body: RigidBody) => void
  unregisterBody: (key: string) => void
  snapshot: () => MachineSnapshot
  loadSnapshot: (snapshot: MachineSnapshot) => void
}

export interface MachineTileContextProviderRef {
  snapshot: MachineTileContextType['snapshot']
  loadSnapshot: MachineTileContextType['loadSnapshot']
}

export const MachineTileContext = createContext<MachineTileContextType>({
  bounds: [0, 0, 0, 0],
  width: 0,
  height: 0,
  registerBody: noop,
  unregisterBody: noop,
  snapshot: () => ({ widgets: {}, balls: [] }),
  loadSnapshot: noop,
})

export const MachineTileContextProvider = forwardRef(
  function MachineTileContextProvider(
    {
      bounds,
      initialSnapshot,
      children,
    }: {
      bounds: Bounds
      initialSnapshot?: MachineSnapshot
      children: ReactNode
    },
    ref,
  ) {
    const { snapshotBalls, restoreBalls, destroyBall } = useMachine()

    const [x1, y1, x2, y2] = bounds
    const stableBounds = useMemo(() => [x1, y1, x2, y2], [x1, x2, y1, y2])

    const width = x2 - x1
    const height = y2 - y1

    const activeBodies = useRef<ActiveBodies>({})

    const registerBody = useCallback((key: string, body: RigidBody) => {
      activeBodies.current[key] = body
    }, [])

    const unregisterBody = useCallback((key: string) => {
      delete activeBodies.current[key]
    }, [])

    const snapshot = useCallback(() => {
      const [x1, y1, x2, y2] = stableBounds
      const rapierBounds: Bounds = [
        // This is tricky: because rapier's y axis grows in the opposite direction of ours, we swap y1 and y2.
        ...coords.toRapier.vector(x1, y2),
        ...coords.toRapier.vector(x2, y1),
      ]

      return {
        widgets: mapValues(activeBodies.current, (body, key) => ({
          ...snapshotBody(body),
          key,
        })),
        balls: snapshotBalls(rapierBounds),
      }
    }, [snapshotBalls, stableBounds])

    const loadSnapshot = useCallback(
      (snapshot: MachineSnapshot) => {
        const [x1, y1] = stableBounds

        // Tile snapshots are based on a zero origin. We must offset the translation coordinates to our tile position in rapier space.
        const [rapierOffsetX, rapierOffsetY] = coords.toRapier.vector(x1, y1)

        for (const [id, widgetSnapshot] of Object.entries(snapshot.widgets)) {
          const body = activeBodies.current[id]
          if (body) {
            applySnapshotToBody(
              offsetSnapshot(rapierOffsetX, rapierOffsetY, widgetSnapshot),
              body,
            )
          }
        }

        restoreBalls(
          snapshot.balls.map((snapshot) =>
            offsetSnapshot(rapierOffsetX, rapierOffsetY, snapshot),
          ),
        )
      },
      [restoreBalls, stableBounds],
    )

    const contextValue = useMemo(
      () => ({
        bounds,
        width,
        height,
        registerBody,
        unregisterBody,
        snapshot,
        loadSnapshot,
      }),
      [
        bounds,
        width,
        height,
        registerBody,
        unregisterBody,
        snapshot,
        loadSnapshot,
      ],
    )

    useImperativeHandle(ref, () => ({
      snapshot,
      loadSnapshot,
    }))

    const hasRestoredSnapshot = useRef(false)
    useRapierEffect(
      ({ world, rapier: { Cuboid } }) => {
        if (initialSnapshot && !hasRestoredSnapshot.current) {
          loadSnapshot(initialSnapshot)
          hasRestoredSnapshot.current = true
        }

        // Clean up balls when tiles removed
        return () => {
          world.intersectionsWithShape(
            coords.toRapier.vectorObject(x1 + width / 2, y1 + height / 2),
            0,
            new Cuboid(...coords.toRapier.lengths(width / 2, height / 2)),
            (collider) => {
              const body = collider.parent()
              if (!body || !isBall(body)) {
                return true
              }

              destroyBall(body.userData.id)
              return true
            },
          )
        }
      },
      [loadSnapshot, initialSnapshot, x1, width, y1, height, destroyBall],
    )

    return (
      <MachineTileContext.Provider value={contextValue}>
        {children}
      </MachineTileContext.Provider>
    )
  },
)

export function useRigidBody(
  create: (rapier: typeof RAPIER) => {
    key: string | null
    bodyDesc: RigidBodyDesc
    colliderDescs?: ColliderDesc[]
  },
  deps: DependencyList,
) {
  const { registerBody, unregisterBody } = useContext(MachineTileContext)

  const bodyRef = useRef<RigidBody | null>(null)

  useRapierEffect(({ rapier, world }) => {
    const { key, bodyDesc, colliderDescs } = create(rapier)
    const body = world.createRigidBody(bodyDesc)
    bodyRef.current = body

    colliderDescs
      ? colliderDescs.map((colliderDesc) =>
          world.createCollider(colliderDesc, body),
        )
      : null

    if (key != null) {
      registerBody(key, body)
    }

    return () => {
      if (key != null) {
        unregisterBody(key)
      }
      setTimeout(() => {
        world.removeRigidBody(body)
      }, 0)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps)

  return bodyRef
}

export function useSensorInTile(
  collider: Collider | undefined,
  handler: (otherCollider: Collider) => void,
  deps: DependencyList,
) {
  const { bounds } = useContext(MachineTileContext)

  useLoopHandler(
    ({ world }) => {
      if (!collider) {
        return
      }

      world.intersectionPairsWith(collider, (otherCollider) => {
        const [x, y] = coords.fromBody.vector(otherCollider)
        if (inBounds(x, y, bounds)) {
          handler(otherCollider)
        }
      })
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [collider, ...deps],
  )
}

export function useMachineTile() {
  return useContext(MachineTileContext)
}
