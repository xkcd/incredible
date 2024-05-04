import { css } from '@emotion/react'
import { percent } from '../../lib/utils'
import { Puzzle, PuzzlePosition } from '../../types'

const posClassNames = ['blue', 'red', 'green', 'yellow']
const posStyles = css({
  width: 6,
  height: 6,
  zIndex: 20,

  '&.input': {
    borderRadius: '100%',
  },

  '&.blue': {
    background: 'blue',
  },

  '&.red': {
    background: 'red',
  },

  '&.green': {
    background: 'green',
  },

  '&.yellow': {
    background: 'yellow',
  },
})

export function TinyInputOutput({
  x,
  y,
  balls,
  isInput,
  style,
}: {
  isInput?: boolean
  style?: React.CSSProperties
} & PuzzlePosition) {
  return (
    <div
      style={{
        ...style,
        position: 'absolute',
        left: `${percent(x)}`,
        top: `${percent(y)}`,
        transform: 'translate(-50%, -50%)',
      }}
    >
      {balls.map(({ type }, idx) => (
        <div
          key={idx}
          css={posStyles}
          className={`${posClassNames[type - 1]} ${isInput && 'input'}`}
        ></div>
      ))}
    </div>
  )
}

export function ModTileInputOutputView({ puzzle }: { puzzle: Puzzle }) {
  return (
    <>
      {puzzle.inputs.map((input, idx) => (
        <TinyInputOutput key={idx} {...input} isInput />
      ))}
      {puzzle.outputs.map((input, idx) => (
        <TinyInputOutput key={idx} {...input} />
      ))}
    </>
  )
}
