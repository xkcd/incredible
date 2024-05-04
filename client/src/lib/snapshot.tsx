import type { RigidBody, Vector } from '@dimforge/rapier2d'
import { Angled, BallType } from '../types'

export interface BodySnapshot extends Vector, Angled {
  vx: number
  vy: number
  va: number
}

export interface WidgetSnapshot extends BodySnapshot {
  key: string
}

export interface BallSnapshot extends BodySnapshot {
  type: BallType
  age: number
}

export interface MachineSnapshot {
  widgets: Record<string, WidgetSnapshot>
  balls: BallSnapshot[]
}

export function snapshotBody(body: RigidBody): BodySnapshot {
  const { x: vx, y: vy } = body.linvel()
  return {
    ...body.translation(),
    angle: body.rotation(),
    vx,
    vy,
    va: body.angvel(),
  }
}
export function applySnapshotToBody(
  snapshot: BodySnapshot,
  body: RigidBody,
  offsetX = 0,
  offsetY = 0,
) {
  body.setTranslation(
    { x: snapshot.x + offsetX, y: snapshot.y + offsetY },
    true,
  )
  body.setRotation(snapshot.angle, true)
  body.setLinvel({ x: snapshot.vx, y: snapshot.vy }, true)
  body.setAngvel(snapshot.va, true)
}

export function offsetSnapshot<T extends BodySnapshot>(
  xOffset: number,
  yOffset: number,
  { x, y, ...rest }: T,
) {
  return { ...rest, x: x + xOffset, y: y + yOffset }
}
