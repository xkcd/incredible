import { sumBy } from 'lodash'
import { useCallback, useRef } from 'react'
import weighted from 'weighted'
import { PuzzlePosition } from '../../types'
import { CreateBallOptions, useMachine } from '../MachineContext'
import {
  PhysicsContextType,
  useLoopHandler,
  useRapierEffect,
} from '../PhysicsContext'
import { BALL_RADIUS, INPUT_SPINNER_SIZE } from '../constants'
import InputOutput, { InputOutputSide } from './InputOutput'

export const BALL_RATE_VARIANCE = 1 // 50% either direction

export function BallSpawner({
  x,
  y,
  vx,
  vy,
  overrideDamping,
  balls,
}: PuzzlePosition & CreateBallOptions) {
  const { createBall, msPerBall } = useMachine()

  const nextBallTick = useRef(0)

  const scheduleNextBall = useCallback(
    ({ tickMs, getCurrentTick }: PhysicsContextType) => {
      const totalRate = sumBy(balls, ({ rate }) => rate)
      const msPerSpawn = msPerBall / totalRate
      const periodTicks = msPerSpawn / tickMs
      const variance = 1 + (0.5 - Math.random()) * BALL_RATE_VARIANCE
      nextBallTick.current =
        getCurrentTick() + Math.round(periodTicks * variance)
    },
    [msPerBall, balls],
  )

  useRapierEffect(scheduleNextBall, [scheduleNextBall])

  useLoopHandler(
    (physics) => {
      const currentTick = physics.getCurrentTick()
      if (currentTick >= nextBallTick.current) {
        const { type } = weighted(
          balls,
          balls.map(({ rate }) => rate),
        )
        createBall(x, y, type, { vx, vy, overrideDamping })
        scheduleNextBall(physics)
        return
      }
    },
    [createBall, overrideDamping, scheduleNextBall, balls, vx, vy, x, y],
  )

  return null
}

export default function SpawnInput({
  x,
  y,
  balls,
  side,
}: { side: InputOutputSide } & PuzzlePosition) {
  // Side inputs are tricky since we don't have gravity. We spawn the ball to the side with a line sloping down.
  const sideSpawnOffset = 0.75 * INPUT_SPINNER_SIZE
  const xOffset =
    side === 'left' ? -sideSpawnOffset : side === 'right' ? sideSpawnOffset : 0

  const yOffset =
    side === 'top'
      ? -BALL_RADIUS
      : side === 'bottom'
        ? BALL_RADIUS
        : -BALL_RADIUS * 2

  return (
    <>
      <BallSpawner x={x + xOffset} y={y + yOffset} balls={balls} />
      <InputOutput x={x} y={y} balls={balls} side={side} isInput />
    </>
  )
}
