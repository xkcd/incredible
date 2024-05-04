import { useCallback, useEffect, useMemo, useState } from 'react'
import { BallData, PuzzlePosition } from '../../types'
import { ComicImageAnimation } from '../ComicImage'
import { useMachine } from '../MachineContext'
import { getPositionStyles } from '../positionStyles'
import InputOutput, { InputOutputSide } from './InputOutput'
import { BALL_RATE_VARIANCE } from './SpawnInput'

import imgCheck from '@art/check-circle_4x.png'
import imgWrong from '@art/wrong-circle_4x.png'
import { sumBy } from 'lodash'
import { CircleGauge } from './CircleGauge'

const imgs = [imgCheck, imgWrong]

const UPDATE_MS = 1000 / 15
const RATE_WINDOW_MS = 10 * 1000

export default function OutputValidator({
  x,
  y,
  balls: balls,
  side,
  id,
  onValidate,
}: {
  side: InputOutputSide
  id: string
  onValidate: (id: string, isValid: boolean) => void
} & PuzzlePosition) {
  const { msPerBall, exitBall } = useMachine()

  const [gaugeLevel, setGaugeLevel] = useState(0)

  const expectedTypes = useMemo(
    () => new Set(balls.map(({ type }) => type)),
    [balls],
  )

  const totalRate = sumBy(balls, ({ rate }) => rate)
  const msPerSpawn = msPerBall / totalRate
  const maxGauge = RATE_WINDOW_MS / msPerSpawn
  // Allow the gauge to fill 2 balls (+ expected variance) past 100%, so it doesn't immediately drop between receiving balls.
  const maxGaugeOverfill = maxGauge + 3 + 0.5 * BALL_RATE_VARIANCE

  const gaugePercent = gaugeLevel / maxGauge
  const isValid = gaugePercent >= 1

  const handleReceiveBall = useCallback(
    (ballData: BallData) => {
      const delta = expectedTypes.has(ballData.ballType) ? 1 : -1

      setGaugeLevel((curLevel) =>
        // Add 1 + the expected 1 the RATE_WINDOW_MS will decay per ball.
        Math.min(curLevel + delta * 2, maxGaugeOverfill),
      )

      exitBall(ballData.id)
    },
    [exitBall, expectedTypes, maxGaugeOverfill],
  )

  useEffect(() => {
    const interval = setInterval(() => {
      // The gauge decays fully after RATE_WINDOW_MS.
      setGaugeLevel((curLevel) =>
        Math.max(0, curLevel - maxGauge * (UPDATE_MS / RATE_WINDOW_MS)),
      )
    }, UPDATE_MS)
    return () => {
      clearInterval(interval)
    }
  }, [maxGauge, msPerSpawn])

  useEffect(() => {
    onValidate?.(id, isValid)
  }, [id, isValid, onValidate])

  return (
    <>
      <div
        css={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          width: 26,
          height: 26,
          marginLeft: side === 'bottom' ? -32 : side === 'right' ? -40 : 40,
          marginTop: side === 'bottom' ? -40 : -32,
          borderRadius: '100%',
          outline: '2px solid white',
          pointerEvents: 'none',
          opacity: 0.75,
          zIndex: 10,
        }}
        style={getPositionStyles(x, y)}
      >
        <ComicImageAnimation
          css={{
            position: 'absolute',
          }}
          imgs={imgs}
          showIdx={isValid ? 0 : 1}
        />
        <CircleGauge
          value={gaugePercent}
          lineWidth={0.25}
          css={{
            position: 'absolute',
            stroke: isValid ? 'darkgreen' : 'darkred',
            fill: 'transparent',
          }}
        />
      </div>
      <InputOutput
        x={x}
        y={y}
        balls={balls}
        side={side}
        isInput={false}
        onReceiveBall={handleReceiveBall}
      />
    </>
  )
}
