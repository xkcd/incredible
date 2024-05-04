import { groupBy, sortBy } from 'lodash'
import { useCallback, useRef } from 'react'
import { coords } from '../../lib/coords'
import { Puzzle, PuzzlePosition, Sized } from '../../types'
import { useMachineTile } from '../MachineTileContext'
import { useCollider } from '../PhysicsContext'
import { INPUT_WIDTH } from '../constants'
import InputOutput, { InputOutputSide, positionToSide } from './InputOutput'
import OutputValidator from './OutputValidator'
import SpawnInput from './SpawnInput'

function Line({
  left,
  top,
  width,
  height,
}: { left: number; top: number } & Sized) {
  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(width / 2, height / 2))
        .setTranslation(
          ...coords.toRapier.vector(left + width / 2, top + height / 2),
        )
        .setRestitution(0.5),
    [height, width, left, top],
  )

  return (
    <div
      css={{
        background: 'black',
      }}
      style={{
        position: 'absolute',
        left,
        top,
        width,
        height,
      }}
    />
  )
}

function hLinesWithBreaks(
  positions: PuzzlePosition[],
  offsetX: number,
  y: number,
  width: number,
) {
  const line = []
  let left = 0

  const sortedPositions = sortBy(positions, (p) => p.x)
  for (let i = 0; i < sortedPositions.length; i++) {
    const { x } = sortedPositions[i]
    line.push(
      <Line
        key={i}
        left={offsetX + left}
        top={y}
        width={x * width - left - INPUT_WIDTH / 2}
        height={1}
      />,
    )

    left = x * width + INPUT_WIDTH / 2
  }

  line.push(
    <Line
      key="line-end"
      left={offsetX + left}
      top={y}
      width={width - left}
      height={1}
    />,
  )

  return line
}

function vLinesWithBreaks(
  positions: PuzzlePosition[],
  x: number,
  offsetY: number,
  height: number,
) {
  const line = []
  let top = 0

  const sortedPositions = sortBy(positions, (p) => p.y)
  for (let i = 0; i < sortedPositions.length; i++) {
    const { y } = sortedPositions[i]
    line.push(
      <Line
        key={i}
        left={x}
        top={offsetY + top}
        width={1}
        height={y * height - top - INPUT_WIDTH / 2}
      />,
    )

    top = y * height + INPUT_WIDTH / 2
  }

  line.push(
    <Line
      key="line-end"
      left={x}
      top={offsetY + top}
      width={1}
      height={height - top}
    />,
  )

  return line
}

export default function MachineFrame({
  inputs,
  outputs,
  title,
  spawnBallsTop,
  spawnBallsLeft,
  spawnBallsRight,
  validateOutputs,
  onValidate,
}: Pick<Puzzle, 'inputs' | 'outputs'> & {
  title?: string | undefined
  spawnBallsTop?: boolean
  spawnBallsLeft?: boolean
  spawnBallsRight?: boolean
  validateOutputs?: boolean
  onValidate?: (isValid: boolean) => void
}) {
  const {
    bounds: [offsetX, offsetY],
    width,
    height,
  } = useMachineTile()

  const outputStateMap = useRef<Record<string, boolean>>({})
  const prevValid = useRef(false)
  const handleValidate = useCallback(
    (id: string, isValid: boolean) => {
      outputStateMap.current[id] = isValid
      const isAllValid = Object.values(outputStateMap.current).every(Boolean)
      if (isAllValid !== prevValid.current) {
        onValidate?.(isAllValid)
        prevValid.current = isAllValid
      }
    },
    [onValidate],
  )

  const OutputComponent = validateOutputs ? OutputValidator : InputOutput

  const inputGroups = groupBy(inputs, positionToSide)
  const outputGroups = groupBy(outputs, positionToSide)

  function renderInputs(
    side: InputOutputSide,
    sideInputs: PuzzlePosition[] | undefined,
  ) {
    if (!sideInputs) {
      return null
    }
    return sideInputs.map(({ x, y, balls }, idx) => (
      <SpawnInput
        key={`${side}-${idx}`}
        x={offsetX + width * x}
        y={offsetY + height * y}
        balls={balls}
        side={side}
      />
    ))
  }

  return (
    <>
      {title && (
        <div
          css={{
            position: 'absolute',
            fontFamily: 'xkcd-Regular-v3',
            fontSize: 16,
            maxWidth: width - 6,
            marginLeft: 6,
            marginTop: 4,
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
            textShadow: '2px 0 white, -2px 0 white, 0 -2px white, 0 2px white',
            zIndex: 1,
          }}
          style={{
            left: offsetX,
            top: offsetY,
          }}
        >
          &ldquo;{title}&rdquo;
        </div>
      )}

      {spawnBallsTop && renderInputs('top', inputGroups['top'])}
      {spawnBallsLeft && renderInputs('left', inputGroups['left'])}
      {spawnBallsRight && renderInputs('right', inputGroups['right'])}

      {Object.entries(outputGroups).flatMap(([side, outputs]) =>
        outputs.map(({ x, y, balls }, idx) => (
          <OutputComponent
            key={`${side}-${idx}`}
            id={`${side}-${idx}`}
            x={offsetX + width * x}
            y={offsetY + height * y}
            balls={balls}
            isInput={false}
            side={side as InputOutputSide}
            onValidate={handleValidate}
          />
        )),
      )}

      {hLinesWithBreaks(inputGroups.top ?? [], offsetX, offsetY, width)}
      {hLinesWithBreaks(
        outputGroups.bottom ?? [],
        offsetX,
        offsetY + height - 1,
        width,
      )}
      {vLinesWithBreaks(
        [...(inputGroups.left ?? []), ...(outputGroups.left ?? [])],
        offsetX,
        offsetY,
        height,
      )}
      {vLinesWithBreaks(
        [...(inputGroups.right ?? []), ...(outputGroups.right ?? [])],
        offsetX + width - 1,
        offsetY,
        height,
      )}
    </>
  )
}
