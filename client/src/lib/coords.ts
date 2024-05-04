import type { Collider, RigidBody } from '@dimforge/rapier2d'
import type { Vector } from '../types'
import { Basis } from './utils'

export const M_PER_PX = 1 / 50

export const coords = {
  toRapier: {
    x(distance: number) {
      return distance * M_PER_PX
    },

    y(distance: number) {
      return -distance * M_PER_PX
    },

    length(this: void, length: number) {
      return length * M_PER_PX
    },

    lengths<T extends number[]>(...lengths: T): T {
      return lengths.map(this.length) as T
    },

    vector(
      x: number,
      y: number,
      { xBasis, yBasis, scale = 1 }: Basis = { xBasis: 0, yBasis: 0, scale: 1 },
    ): [number, number] {
      return [this.x((x - xBasis) / scale), this.y((y - yBasis) / scale)]
    },

    vectorObject(
      x: number,
      y: number,
      { xBasis, yBasis, scale = 1 }: Basis = { xBasis: 0, yBasis: 0, scale: 1 },
    ): Vector {
      return {
        x: this.x((x - xBasis) / scale),
        y: this.y((y - yBasis) / scale),
      }
    },

    angle(angle: number) {
      return -angle
    },
  },

  fromRapier: {
    x(distance: number) {
      return distance / M_PER_PX
    },

    y(distance: number) {
      return -distance / M_PER_PX
    },

    length(this: void, length: number) {
      return length / M_PER_PX
    },

    vector(x: number, y: number): [number, number] {
      return [this.x(x), this.y(y)]
    },

    angle(angle: number) {
      return -angle
    },
  },

  fromBody: {
    vector(body: RigidBody | Collider) {
      const { x, y } = body.translation()
      return coords.fromRapier.vector(x, y)
    },

    angle(body: RigidBody | Collider) {
      return coords.fromRapier.angle(body.rotation())
    },
  },
}

export function vectorMagnitude(a: Vector) {
  return Math.sqrt(a.x * a.x + a.y * a.y)
}

export function vectorDistance(a: Vector, b: Vector) {
  return Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2))
}

export function vectorAngle(a: Vector, b: Vector) {
  return Math.atan2(b.y - a.y, b.x - a.x)
}

export function vectorDifference(a: Vector, b: Vector): Vector {
  return { x: a.x - b.x, y: a.y - b.y }
}

export function vectorNorm(a: Vector): Vector {
  const invLength = 1.0 / Math.sqrt(a.x * a.x + a.y * a.y)

  return { x: a.x * invLength, y: a.y * invLength }
}

export function vectorScale(a: Vector, scale: number): Vector {
  return { x: a.x * scale, y: a.y * scale }
}

export function vectorRotate(toRotate: Vector, angleDegrees: number): Vector {
  const angle = angleDegrees * (Math.PI / 180.0)
  const cosA = Math.cos(angle)
  const sinA = Math.cos(angle)
  const { x, y } = toRotate

  return { x: cosA * x - sinA * y, y: sinA * x + cosA * y }
}
