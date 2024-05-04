import ballBlueImg from '@art/ball-blue_4x.png'
import ballGreenImg from '@art/ball-green_4x.png'
import ballRedImg from '@art/ball-red_4x.png'
import ballYellowImg from '@art/ball-yellow_4x.png'
import type RAPIER from '@dimforge/rapier2d'
import type { World } from '@dimforge/rapier2d'
import { ClassNames, css } from '@emotion/react'
import useLatest from '@react-hook/latest'
import { sample } from 'lodash'
import { MutableRefObject, useRef } from 'react'
import { coords } from '../../lib/coords'
import { inBoundsOutset, px } from '../../lib/utils'
import { UserData } from '../../types'
import {
  BallData,
  BallDestroyReason,
  BallFollowCallback,
  MachineContextType,
  useMachine,
} from '../MachineContext'
import { useRapierEffect } from '../PhysicsContext'
import {
  BALL_RADIUS,
  BOTTOM_CHUTE_DROP,
  BOTTOM_CHUTE_HEIGHT,
} from '../constants'

type BallRecord = BallData & { el: HTMLDivElement }

interface BallActor {
  step: () => void
  destroy: (reason?: BallDestroyReason) => void
  exit: () => void
  renew: () => void
  die: () => void
  follow: (callback: BallFollowCallback) => void
  unfollow: () => void
}

export const BASE_BALL_LIFETIME_TICKS = 30 * 60 // 30 seconds at 60tps

const ballStyles = css({
  position: 'absolute',
  left: 0,
  top: 0,
  pointerEvents: 'none',
  width: 2 * BALL_RADIUS,
  height: 2 * BALL_RADIUS,
  backgroundSize: 'contain',
  zIndex: 5,
  contain: 'strict',
  '&.blue': {
    backgroundImage: `url(${ballBlueImg.url['2x']})`,
  },
  '&.red': {
    backgroundImage: `url(${ballRedImg.url['2x']})`,
  },
  '&.green': {
    backgroundImage: `url(${ballGreenImg.url['2x']})`,
  },
  '&.yellow': {
    backgroundImage: `url(${ballYellowImg.url['2x']})`,
  },
})

export const ballClassNames = ['blue', 'red', 'green', 'yellow']

function runBall(
  { id, type, age, snapshot, overrideDamping, el }: BallRecord,
  ballClassName: string,
  { RigidBodyDesc, RigidBodyType, ColliderDesc }: typeof RAPIER,
  world: World,
  machineRef: MutableRefObject<MachineContextType>,
  getCurrentTick: () => number,
  lifetimeTicks: number,
): BallActor {
  el.className = ballClassName

  const bodyDesc = new RigidBodyDesc(RigidBodyType.Dynamic)
    .setTranslation(snapshot.x, snapshot.y)
    .setRotation(snapshot.angle)
    .setLinvel(snapshot.vx, snapshot.vy)
    .setAngvel(snapshot.va)
    .setCcdEnabled(true)
    .setUserData({ type: 'BallData', id, ballType: type } satisfies UserData)

  const colliderDesc = ColliderDesc.ball(coords.toRapier.length(BALL_RADIUS))

  if (type === 2) {
    colliderDesc.setRestitution(0.8).setDensity(1)
  }

  if (type === 3) {
    colliderDesc.setMass(0.75)
  }

  if (type === 4) {
    bodyDesc.setLinearDamping(2)
    colliderDesc.setDensity(0.3).setRestitution(0.6)
  }

  if (overrideDamping != null) {
    bodyDesc.setLinearDamping(overrideDamping)
  }

  const body = world.createRigidBody(bodyDesc)
  const collider = world.createCollider(colliderDesc, body)

  let wasVisible = true
  let isDying = false
  let isExiting = false
  let followCallback: BallFollowCallback | null = null
  let renewTick = getCurrentTick() - age

  machineRef.current.registerBall(id, type, body, renewTick)

  const actor: BallActor = {
    step() {
      const { simulationBoundsRef, viewBoundsRef } = machineRef.current

      const ballAge = getCurrentTick() - renewTick
      if (ballAge >= lifetimeTicks && !followCallback) {
        this.die()
      }

      const [x, y] = coords.fromBody.vector(body)

      followCallback?.(x, y)

      if (
        !inBoundsOutset(
          x,
          y,
          BOTTOM_CHUTE_DROP + BOTTOM_CHUTE_HEIGHT,
          simulationBoundsRef.current,
        )
      ) {
        machineRef.current?.destroyBall(id)
        el.style.display = 'none'
        return
      }

      const isVisible = inBoundsOutset(x, y, BALL_RADIUS, viewBoundsRef.current)

      if (isVisible != wasVisible) {
        el.style.display = isVisible ? 'block' : 'none'
      }
      if (!isVisible && isExiting) {
        machineRef.current?.destroyBall(id)
        return
      }
      if (isVisible) {
        el.style.transform = `translate(${px(x - BALL_RADIUS)}, ${px(y - BALL_RADIUS)})`
      }
      wasVisible = isVisible
    },

    destroy() {
      el.remove()
      machineRef.current.unregisterBall(id)

      // Immediately destroying the body seems to cause panics when iterating over colliders.
      setTimeout(() => {
        world.removeRigidBody(body)
      }, 0)
    },

    exit() {
      isExiting = true
    },

    renew() {
      renewTick = getCurrentTick()
      machineRef.current.registerBall(id, type, body, renewTick)
    },

    die() {
      if (isDying) {
        return
      }
      isDying = true

      el.style.transition = 'opacity 200ms ease-in'
      el.style.opacity = '0'

      setTimeout(() => {
        world.removeCollider(collider, false)
      }, 0)

      setTimeout(() => {
        machineRef.current?.destroyBall(id, 'expiry')
      }, 200)
    },

    follow(callback: BallFollowCallback) {
      followCallback = callback
    },

    unfollow() {
      followCallback = null
    },
  }

  actor.step()

  return actor
}

export function BallsRunner({
  lifetimeTicks,
  ballClassName,
}: {
  lifetimeTicks: number
  ballClassName: string
}) {
  const machine = useMachine()
  const parentRef = useRef<HTMLDivElement>(null)

  const { events } = machine
  const machineRef = useLatest(machine)

  useRapierEffect(
    ({ events: worldEvents, rapier, world, getCurrentTick }) => {
      const actors: Record<string, BallActor> = {}
      let followingId: string | undefined = undefined

      function handleCreateBall(ball: BallData) {
        const el = document.createElement('div')
        actors[ball.id] = runBall(
          { ...ball, el },
          `${ballClassName} ${ballClassNames[ball.type - 1]}`,
          rapier,
          world,
          machineRef,
          getCurrentTick,
          lifetimeTicks,
        )
        parentRef.current?.appendChild(el)
      }

      function handleDestroyBall(id: string) {
        actors[id]?.destroy()
        delete actors[id]
      }

      function handleExitBall(id: string) {
        actors[id]?.exit()
      }

      function handleRenewBall(id: string) {
        actors[id]?.renew()
      }

      function handleKillBall(id: string) {
        actors[id]?.die()
      }

      function handleFollowBall(callback: BallFollowCallback) {
        handleUnfollowBall()
        followingId = sample(Object.keys(actors))
        if (!followingId) {
          return
        }
        actors[followingId].follow(callback)
      }

      function handleUnfollowBall() {
        if (followingId && actors[followingId]) {
          actors[followingId].unfollow()
        }
      }

      function handleStep() {
        Object.values(actors).forEach((actor) => actor.step())
      }

      events.on('createBall', handleCreateBall)
      events.on('destroyBall', handleDestroyBall)
      events.on('exitBall', handleExitBall)
      events.on('renewBall', handleRenewBall)
      events.on('killBall', handleKillBall)
      events.on('followBall', handleFollowBall)
      events.on('unfollowBall', handleUnfollowBall)
      worldEvents.on('step', handleStep)

      return () => {
        events.off('createBall', handleCreateBall)
        events.off('destroyBall', handleDestroyBall)
        events.off('exitBall', handleExitBall)
        events.off('renewBall', handleRenewBall)
        events.off('killBall', handleKillBall)
        events.off('followBall', handleFollowBall)
        events.off('unfollowBall', handleUnfollowBall)
        worldEvents.off('step', handleStep)
        Object.values(actors).forEach((actor) => actor.destroy())
      }
    },
    [ballClassName, events, lifetimeTicks, machineRef],
  )

  return <div ref={parentRef} />
}

export function Balls({
  lifetimeTicks = BASE_BALL_LIFETIME_TICKS,
}: {
  lifetimeTicks?: number
}) {
  return (
    <ClassNames>
      {({ css }) => (
        <BallsRunner
          lifetimeTicks={lifetimeTicks}
          ballClassName={css(ballStyles)}
        />
      )}
    </ClassNames>
  )
}
