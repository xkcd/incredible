import { Puzzle, WidgetCollection } from '../../types'
import { Widgets } from '../widgets'
import { Balls } from '../widgets/Balls'
import MachineFrame from '../widgets/MachineFrame'
import { ModTileInputOutputView } from './ModTileInputOutputView'
import { ServerBlueprint } from './modTypes'

export default function ModMachineTileView({
  puzzle,
  blueprint,
  width,
  height,
  tileWidth,
  tileHeight,
  onValidate,
  className,
}: {
  puzzle: Puzzle
  blueprint?: ServerBlueprint
  width: number
  height: number
  tileWidth: number
  tileHeight: number
  onValidate?: (isValid: boolean) => void
  className?: string
}) {
  return (
    <div css={{ position: 'relative', width, height, overflow: 'hidden' }}>
      <div
        css={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: tileWidth,
          height: tileHeight,
          transform: `scale(${width / tileWidth})`,
          transformOrigin: 'left top',
          aspectRatio: '1',
        }}
        className={className}
      >
        {blueprint ? (
          <Widgets widgets={blueprint.widgets as WidgetCollection} />
        ) : null}
        {puzzle ? (
          <MachineFrame
            key={blueprint?.puzzle}
            inputs={puzzle.inputs}
            outputs={puzzle.outputs}
            onValidate={onValidate}
            validateOutputs={onValidate != null}
            spawnBallsTop
            spawnBallsLeft
            spawnBallsRight
          />
        ) : null}
        <Balls />
      </div>
      <ModTileInputOutputView puzzle={puzzle} />
    </div>
  )
}
