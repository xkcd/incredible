import type { RigidBody, Vector } from '@dimforge/rapier2d'
import { isNumber, isString } from 'lodash'
import { WidgetData } from './components/widgets'

export type { Vector }

export interface Sized {
  width: number
  height: number
}

export interface Angled {
  angle: number
}

export interface OutputLoc {
  pos: Vector
}

export type BallType = number
export type BallTypeRate = { type: BallType; rate: number }
export type PuzzlePosition = { balls: BallTypeRate[] } & Vector

export interface Puzzle {
  inputs: PuzzlePosition[]
  outputs: PuzzlePosition[]
}

export interface PuzzleOrder extends Puzzle {
  id: string
  workOrder: string
}

export type WidgetCollection = Record<string, WidgetData>

export type Bounds = [x1: number, y1: number, x2: number, y2: number]

export type BallData = {
  type: 'BallData'
  id: string
  ballType: BallType
}

export type UserData = BallData & { type: unknown }

export type Ball = RigidBody & { userData: BallData }

export function isBall(body: RigidBody): body is Ball {
  const userData = body.userData
  if (userData == null) {
    return false
  }
  const ballData = userData as BallData
  return (
    ballData.type === 'BallData' &&
    isNumber(ballData.ballType) &&
    isString(ballData.id)
  )
}
