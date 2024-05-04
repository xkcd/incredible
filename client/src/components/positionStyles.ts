import type { RigidBody } from '@dimforge/rapier2d'
import {
  MutableRefObject,
  RefObject,
  useCallback,
  useEffect,
  useRef,
} from 'react'
import { coords } from '../lib/coords'
import { inBoundsObject, px } from '../lib/utils'
import { useMachine } from './MachineContext'
import { useLoopHandler } from './PhysicsContext'

export function getPositionStyles(x: number, y: number, angle: number = 0) {
  return {
    position: 'absolute' as const,
    top: 0,
    left: 0,
    transform: `translate(${px(x)}, ${px(y)}) translate(-50%, -50%) rotate(${angle}rad)`,
  }
}

export function usePositionedBodyRef<T extends HTMLElement>(
  bodyRef: RefObject<RigidBody>,
  {
    width,
    height,
    initialX,
    initialY,
    initialAngle,
    xBasis = 0,
    yBasis = 0,
  }: {
    width: number
    height: number
    initialX: number
    initialY: number
    initialAngle?: number
    xBasis?: number
    yBasis?: number
  },
): MutableRefObject<T | null> {
  const elRef = useRef<T>(null)

  const { viewBoundsRef } = useMachine()

  const wasVisibleRef = useRef(true)

  const updateStyle = useCallback(() => {
    const { current: el } = elRef
    const { current: body } = bodyRef

    if (!el) {
      return
    }

    const [bodyX, bodyY] = body ? coords.fromBody.vector(body) : []
    const x = bodyX ?? initialX
    const y = bodyY ?? initialY
    if (x == null || y == null) {
      el.style.display = 'none'
      wasVisibleRef.current = false
      return
    }

    const isVisible = inBoundsObject(x, y, width, height, viewBoundsRef.current)
    if (isVisible != wasVisibleRef.current) {
      el.style.display = isVisible ? 'block' : 'none'
    }
    if (isVisible) {
      Object.assign(
        el.style,
        getPositionStyles(
          x - xBasis,
          y - yBasis,
          body ? coords.fromBody.angle(body) : initialAngle,
        ),
      )
    }
    wasVisibleRef.current = isVisible
  }, [
    bodyRef,
    initialX,
    initialY,
    initialAngle,
    width,
    height,
    viewBoundsRef,
    xBasis,
    yBasis,
  ])

  useEffect(updateStyle)
  useLoopHandler(updateStyle, [updateStyle])

  return elRef
}
