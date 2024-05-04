import { useCallback, useMemo, useRef } from 'react'
import { coords, vectorAngle } from '../lib/coords'
import { inBounds } from '../lib/utils'
import { Ball, BallType, Bounds } from '../types'
import {
  MachineContextProvider,
  MachineContextProviderRef,
  useMachine,
} from './MachineContext'
import { GRAVITY, PhysicsContextProvider } from './PhysicsContext'
import {
  BOTTOM_CHUTE_DROP,
  BOTTOM_CHUTE_EXIT_OFFSET,
  BOTTOM_MECHANISM_HEIGHT,
  BOTTOM_PIT_HEIGHT,
  BOTTOM_TANK_HEIGHT,
  BOTTOM_TANK_SPACING,
  BOTTOM_TANK_TARGET_TOP,
  BOTTOM_TANK_WIDTH,
} from './constants'
import { useMetaMachineClient } from './useMetaMachineClient'
import { BASE_BALL_LIFETIME_TICKS, Balls } from './widgets/Balls'
import Boat from './widgets/Boat'
import { BottomChute } from './widgets/BottomChute'
import { BottomPit } from './widgets/BottomPit'
import { BottomTank } from './widgets/BottomTank'
import { BallSpawner } from './widgets/SpawnInput'

const tankIdxByType = [2, 0, 3, 1]

/**
 * Bottom pool with balls fed by chutes at the bottom of the puzzle.
 *
 */
export function BallPitMechanism({
  tilesX,
  tilesY,
  tileWidth,
  tileHeight,
  stepRateMultiplier = 1,
}: {
  tilesX: number
  tilesY: number
  tileWidth: number
  tileHeight: number
  stepRateMultiplier?: number
}) {
  const { msPerBall, simulationBoundsRef } = useMachine()
  const subMachineRef = useRef<MachineContextProviderRef>(null)

  const totalWidth = tilesX * tileWidth
  const totalHeight = tilesY * tileHeight
  const pitX = totalWidth / 2
  const pitY =
    totalHeight + BOTTOM_MECHANISM_HEIGHT - BOTTOM_PIT_HEIGHT / 2 - 10
  const tankCount = 4
  const firstTankCenterX =
    pitX -
    (4 * BOTTOM_TANK_WIDTH + 3 * BOTTOM_TANK_SPACING) / 2 +
    BOTTOM_TANK_WIDTH / 2
  const tankTopY = pitY - BOTTOM_TANK_HEIGHT - 400

  const lastLineBounds: Bounds = [
    0,
    (tilesY - 1) * tileHeight,
    totalWidth,
    tilesY * tileHeight,
  ]

  const bounds: Bounds = useMemo(
    () => [0, 0, totalWidth, totalHeight + BOTTOM_MECHANISM_HEIGHT],
    [totalHeight, totalWidth],
  )

  // Load the last row of machines
  const { metaMachine } = useMetaMachineClient({ viewBounds: lastLineBounds })

  /**
   * Velocity of ball to hit the bool target
   */
  const getBallVx = useCallback(
    (x: number, y: number, ballType: BallType) => {
      const tankIdx = tankIdxByType[ballType - 1]
      const targetX =
        firstTankCenterX + tankIdx * (BOTTOM_TANK_WIDTH + BOTTOM_TANK_SPACING)

      const [tx, ty] = coords.toRapier.vector(
        targetX,
        tankTopY + BOTTOM_TANK_TARGET_TOP,
      )

      return (tx - x) / Math.sqrt((2 * (y - ty)) / -GRAVITY.y)
    },
    [firstTankCenterX, tankTopY],
  )

  // When balls are received in the chutes in the meta machine, spawn a ball in our machine targeting the pool.
  const handleReceiveBall = useCallback(
    (ball: Ball) => {
      const { current: subMachine } = subMachineRef
      if (!subMachine) {
        return
      }

      const { x, y } = ball.translation()

      subMachine.createBall(
        ...coords.fromRapier.vector(x, y),
        ball.userData.ballType,
        {
          vx: getBallVx(x, y, ball.userData.ballType),
          overrideDamping: 0,
        },
      )
    },
    [getBallVx],
  )

  if (!metaMachine) {
    return null
  }

  const tanks = []
  for (let tankType = 1; tankType <= tankCount; tankType++) {
    tanks.push(
      <BottomTank
        x={
          firstTankCenterX +
          tankIdxByType[tankType - 1] *
            (BOTTOM_TANK_WIDTH + BOTTOM_TANK_SPACING)
        }
        y={tankTopY + BOTTOM_TANK_HEIGHT / 2}
        type={tankType}
        key={`tank-${tankType}`}
      />,
    )
  }

  const chutes = []
  const ballSpawns = []
  for (let xt = 0; xt < tilesX; xt++) {
    const machine = metaMachine.getMachine(xt, tilesY - 1)
    if (!machine) {
      continue
    }

    for (const output of machine.puzzle.outputs) {
      if (output.y !== 1) {
        continue
      }

      const x = xt * tileWidth + tileWidth * output.x
      const y = totalHeight

      // If the output exists in simulation, put a chute under it, otherwise, proxy it with a spawner.
      if (inBounds(x, y, simulationBoundsRef.current)) {
        chutes.push(
          <BottomChute
            key={`${xt}-${output.x}`}
            x={x}
            y={y + BOTTOM_CHUTE_DROP}
            angle={vectorAngle(
              { x, y: y + BOTTOM_CHUTE_EXIT_OFFSET },
              { x: pitX, y: pitY },
            )}
            onReceiveBall={handleReceiveBall}
          />,
        )
      } else {
        ballSpawns.push(
          <BallSpawner
            key={`${xt}-${output.x}`}
            x={x}
            y={y + BOTTOM_CHUTE_DROP}
            vx={getBallVx(
              ...coords.toRapier.vector(x, y + BOTTOM_CHUTE_DROP),
              output.balls[0]['type'], // No combined outputs at bottom of map
            )}
            overrideDamping={0}
            balls={output.balls}
          />,
        )
      }
    }
  }

  return (
    <>
      {chutes}
      <PhysicsContextProvider stepRateMultiplier={stepRateMultiplier}>
        <MachineContextProvider
          ref={subMachineRef}
          msPerBall={msPerBall}
          initialSimulationBounds={bounds}
          initialViewBounds={bounds}
        >
          {ballSpawns}
          {tanks}
          <Boat id="boat" x={pitX} y={pitY - 10} />
          <BottomPit x={pitX} y={pitY} />
          <Balls lifetimeTicks={3.5 * BASE_BALL_LIFETIME_TICKS} />
        </MachineContextProvider>
      </PhysicsContextProvider>
    </>
  )
}
