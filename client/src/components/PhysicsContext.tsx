import type RAPIER from '@dimforge/rapier2d'
import {
  EventQueue,
  type Collider,
  type ColliderDesc,
  type ImpulseJoint,
  type JointData,
  type RigidBody,
  type World,
} from '@dimforge/rapier2d'
import useLatest from '@react-hook/latest'
import { random } from 'lodash'
import mitt, { Emitter } from 'mitt'
import {
  DependencyList,
  EffectCallback,
  ReactNode,
  createContext,
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react'
import { ms } from '../lib/utils'

type LoopEvents = {
  step: void
} & {
  [K in `collision_start_${number}` | `collision_end_${number}`]: number
}

export const GRAVITY = { x: 0.0, y: -9.81 }
export const TICK_MS = 1000 / 60

export interface PhysicsContextType {
  tickMs: number
  rapier: typeof RAPIER
  world: World
  events: Emitter<LoopEvents>
  getCurrentTick: () => number
}

export const PhysicsContext = createContext<PhysicsContextType | null>(null)

export function PhysicsContextProvider({
  stepRateMultiplier = 1,
  debug,
  children,
}: {
  stepRateMultiplier?: number
  debug?: boolean
  children: ReactNode
}) {
  const [contextValue, setContextValue] = useState<PhysicsContextType | null>(
    null,
  )

  const eventQueueRef = useRef<EventQueue | undefined>()
  const baseTimeRef = useRef(0)
  const tickCountRef = useRef(0)
  const isDebugRef = useLatest(debug)

  const getCurrentTick = useCallback(() => tickCountRef.current, [])

  // TODO: pause when page inactive
  useEffect(() => {
    let destroyed = false
    let world: World | undefined

    async function loadAndInit() {
      const rapier = await import('@dimforge/rapier2d')
      if (destroyed) {
        return
      }

      const world = new rapier.World(GRAVITY)
      world.numSolverIterations = 4
      world.timestep = TICK_MS / 1000
      const events = mitt<LoopEvents>()
      eventQueueRef.current = new rapier.EventQueue(true)
      setContextValue({
        tickMs: TICK_MS,
        rapier,
        world,
        events,
        getCurrentTick,
      })
    }

    void loadAndInit()

    return () => {
      destroyed = true
      world?.free()
    }
  }, [getCurrentTick, isDebugRef])

  const resetBaseTime = useCallback(
    (now: number, tickMs: number) => {
      const newBaseTime = now - (tickCountRef.current + 1) * tickMs
      if (isDebugRef.current) {
        console.debug(`Skipping ${newBaseTime - baseTimeRef.current}ms`)
      }
      baseTimeRef.current = newBaseTime
    },
    [isDebugRef],
  )

  useEffect(() => {
    if (stepRateMultiplier === 0) {
      return
    }

    let lastTickTime = performance.now()

    const tickMs = Math.floor(TICK_MS * (1 / stepRateMultiplier))
    resetBaseTime(lastTickTime, tickMs)

    let raf: number = -1
    function step() {
      if (!contextValue) {
        return
      }

      const { current: eventQueue } = eventQueueRef
      const { world, events } = contextValue
      if (!eventQueue || !world) {
        return
      }

      const now = performance.now()

      // Skip forward in case of large pauses.
      if (now - lastTickTime > 30 * tickMs) {
        resetBaseTime(now, tickMs)
      }

      const neededTicks = Math.ceil((now - baseTimeRef.current) / tickMs)

      while (tickCountRef.current < neededTicks) {
        const rapierStart = performance.now()
        world.step(eventQueue)
        const rapierEnd = performance.now()

        const eventQueueCallbackStart = performance.now()
        eventQueue.drainCollisionEvents((handle1, handle2, started) => {
          const eventName = started ? 'start' : 'end'
          events.emit(`collision_${eventName}_${handle1}`, handle2)
          events.emit(`collision_${eventName}_${handle2}`, handle1)
        })
        const eventQueueCallbackEnd = performance.now()

        const stepCallbackStart = performance.now()
        events.emit('step')
        const stepCallbackEnd = performance.now()

        if (isDebugRef.current && tickCountRef.current % 60 === 0) {
          console.debug({
            rapier: ms(rapierEnd - rapierStart),
            eventQueue: ms(eventQueueCallbackEnd - eventQueueCallbackStart),
            stepCallback: ms(stepCallbackEnd - stepCallbackStart),
            bodies: world.bodies.len(),
          })
        }

        tickCountRef.current++
        lastTickTime = performance.now()
      }

      raf = requestAnimationFrame(step)
    }

    raf = requestAnimationFrame(step)

    return () => {
      cancelAnimationFrame(raf)
    }
  }, [contextValue, isDebugRef, resetBaseTime, stepRateMultiplier])

  return (
    <PhysicsContext.Provider value={contextValue}>
      {children}
    </PhysicsContext.Provider>
  )
}

export function useRapierEffect(
  rapierEffect: (physics: PhysicsContextType) => ReturnType<EffectCallback>,
  deps: DependencyList,
) {
  const physics = useContext(PhysicsContext)

  useEffect(() => {
    if (!physics) {
      return
    }

    return rapierEffect(physics)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [physics, ...deps])
}

export function useCollider(
  create: (rapier: typeof RAPIER) => ColliderDesc | null,
  deps: DependencyList,
) {
  // TODO: needed to make this a setState for useCollisionHandler to get collider object updates because of the useRapierEffect updates the ref async. Ideally this would be solved without triggering a render. A JSX component-based approach would be more ergonomic than the hooks here.
  const [collider, setCollider] = useState<Collider>()

  useRapierEffect(({ rapier, world }) => {
    const colliderDesc = create(rapier)
    if (!colliderDesc) {
      return
    }
    const collider = world.createCollider(colliderDesc)
    setCollider(collider)
    return () => {
      setTimeout(() => {
        world.removeCollider(collider, false)
      }, 0)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps)

  return collider
}

// TODO: Test this! It looks right but I haven't tested it <.<  >.>
export function useImpulseJoint(
  create: (rapier: typeof RAPIER) => JointData | null,
  body1Ref: React.MutableRefObject<RigidBody | null>,
  body2Ref: React.MutableRefObject<RigidBody | null>,
  deps: DependencyList,
) {
  const jointRef = useRef<ImpulseJoint | null>(null)
  useRapierEffect(
    ({ rapier, world }) => {
      const jointDesc = create(rapier)
      const { current: left } = body1Ref
      const { current: right } = body2Ref

      if (!jointDesc || !left || !right) {
        return
      }

      const joint = world.createImpulseJoint(jointDesc, left, right, false)
      jointRef.current = joint

      return () => {
        setTimeout(() => {
          world.removeImpulseJoint(joint, false)
        }, 0)
      }
    },
    // don't support changing 'create' since it'll break the deps array on every render =(
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [deps, body1Ref, body2Ref],
  )

  return jointRef
}

export function useLoopHandler(
  handler: (physics: PhysicsContextType) => void,
  deps: DependencyList,
  enabled: boolean = true,
) {
  useRapierEffect((physics) => {
    if (!enabled) {
      return
    }

    const { events } = physics
    function triggerHandler() {
      handler(physics)
    }
    events.on('step', triggerHandler)
    return () => {
      events.off('step', triggerHandler)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps)
}

export function useCollisionHandler(
  event: 'start' | 'end',
  collider: Collider | undefined,
  handler: (otherCollider: Collider) => void,
  deps: DependencyList,
) {
  useRapierEffect(
    (physics) => {
      if (!collider) {
        return
      }

      const { world, events } = physics
      function triggerHandler(handle: number) {
        const otherCollider = world.getCollider(handle)
        handler(otherCollider)
      }

      const { handle } = collider
      events.on(`collision_${event}_${handle}`, triggerHandler)
      return () => {
        events.off(`collision_${event}_${handle}`, triggerHandler)
      }
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [event, collider, ...deps],
  )
}

export function usePhysicsLoaded() {
  return useContext(PhysicsContext) != null
}

export function PhysicsLoader({
  spinner,
  children,
}: {
  spinner: ReactNode
  children: ReactNode
}) {
  const isPhysicsLoaded = usePhysicsLoaded()
  if (!isPhysicsLoaded) {
    return spinner
  }
  return children
}

export function deferATick(callback: () => void) {
  return setTimeout(() => {
    callback()
  }, random(TICK_MS))
}
