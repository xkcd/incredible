import { useCallback, useRef } from 'react'
import { Bounds } from '../types'

export function useIdGen(getInitial?: () => number) {
  const nextIdRef = useRef<number | null>(null)
  if (nextIdRef.current === null) {
    nextIdRef.current = getInitial ? getInitial() : 0
  }
  return useCallback(() => String(nextIdRef.current!++), [])
}

export function px(value: number) {
  return `${value}px`
}

export function percent(value: number) {
  return `${(100 * value).toFixed(2)}%`
}

export function ms(value: number) {
  return `${value.toFixed(1)}ms`
}

export function inBounds(x: number, y: number, [x1, y1, x2, y2]: Bounds) {
  return x >= x1 && x <= x2 && y >= y1 && y <= y2
}

export function inBoundsOutset(
  x: number,
  y: number,
  outset: number,
  [x1, y1, x2, y2]: Bounds,
) {
  return (
    x >= x1 - outset && x <= x2 + outset && y >= y1 - outset && y <= y2 + outset
  )
}

export function inBoundsObject(
  x: number,
  y: number,
  width: number,
  height: number,
  [x1, y1, x2, y2]: Bounds,
) {
  return (
    x >= x1 - 0.5 * width &&
    x <= x2 + 0.5 * width &&
    y >= y1 - 0.5 * height &&
    y <= y2 + 0.5 * height
  )
}

export function intersectBounds(
  [ax1, ay1, ax2, ay2]: Bounds,
  [bx1, by1, bx2, by2]: Bounds,
): Bounds {
  return [
    Math.max(ax1, bx1),
    Math.max(ay1, by1),
    Math.min(ax2, bx2),
    Math.min(ay2, by2),
  ]
}

export function rotate(
  rotationAngle: number,
  { x, y }: { x: number; y: number },
) {
  return {
    x: x * Math.cos(rotationAngle) - y * Math.sin(rotationAngle),
    y: x * Math.sin(rotationAngle) + y * Math.cos(rotationAngle),
  }
}

export interface Basis {
  xBasis: number
  yBasis: number
  scale?: number
}

export interface RandallPath {
  x1: number
  y1: number
  x2: number
  y2: number
  thickness: number
}
