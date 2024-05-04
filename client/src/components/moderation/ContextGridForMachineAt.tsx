import { css } from '@emotion/react'
import { gridDimensions, iterTiles, tileKey } from '../../lib/tiles'
import { inBounds } from '../../lib/utils'
import { Puzzle } from '../../types'
import LoadingSpinner from '../LoadingSpinner'
import ModMachineTileView from './ModMachineTileView'
import { ModTileInputOutputView } from './ModTileInputOutputView'
import { ModLocation, ModMachine, ServerBlueprint } from './modTypes'
import { useContextBlueprints, useContextPuzzles } from './moderatorClient'

const TILE_WIDTH = 100

const tileWrapperStyles = css({
  outline: '1px solid rgba(0, 0, 0, .35)',
  userSelect: 'none',
})

const centerTextStyles = css({
  display: 'flex',
  justifyContent: 'center',
  alignItems: 'center',
  width: '100%',
  height: '100%',
})

function OOBTile() {
  return (
    <div
      css={[
        { color: 'gainsboro', backgroundColor: 'slategrey' },
        tileWrapperStyles,
        centerTextStyles,
      ]}
    >
      OUT OF
      <br />
      BOUNDS
    </div>
  )
}

function EmptyTile({
  puzzle,
  toModCount,
  onClick,
}: {
  puzzle: Puzzle
  toModCount: number | undefined
  onClick?: () => void
}) {
  return (
    <div
      onClick={onClick}
      css={[
        {
          position: 'relative',
          backgroundColor: 'gainsboro',
          overflow: 'hidden',
        },
        tileWrapperStyles,
        centerTextStyles,
      ]}
    >
      {toModCount}
      <ModTileInputOutputView puzzle={puzzle} />
    </div>
  )
}

export default function ContextGridForMachineAt({
  modMachine,
  xt: viewXt,
  yt: viewYt,
  selectedBlueprint,
  tileOutset = 2,
  onSelectLocation,
}: {
  modMachine: ModMachine
  selectedBlueprint: ServerBlueprint | undefined
  tileOutset?: number
  onSelectLocation: (xt: number, yt: number) => void
} & ModLocation) {
  const sideSize: number = tileOutset * 2 + 1

  const [tilesX, tilesY] = gridDimensions(modMachine.grid)

  const contextLocs: Array<
    | (ModLocation & { oob: false; toModCount: number | undefined })
    | {
        xt: number
        yt: number
        oob: true
      }
  > = Array.from(
    iterTiles(
      viewXt - tileOutset,
      viewYt - tileOutset,
      viewXt + tileOutset,
      viewYt + tileOutset,
    ),
    ([xt, yt]) =>
      inBounds(xt, yt, [0, 0, tilesX - 1, tilesY - 1])
        ? {
            xt,
            yt,
            oob: false,
            puzzle: modMachine.grid[yt][xt].puzzle,
            blueprint: modMachine.grid[yt][xt].blueprint,
            toModCount: modMachine.grid[yt][xt].to_mod,
          }
        : { xt, yt, oob: true },
  )

  const puzzles = useContextPuzzles(
    contextLocs.map((l) => (l.oob ? undefined : l.puzzle)),
  )
  const blueprints = useContextBlueprints(
    contextLocs.map((l) => (l.oob ? undefined : l.blueprint)),
  )

  const contextTiles = contextLocs.map((loc, idx) => {
    const { xt, yt, oob } = loc
    const key = tileKey(xt, yt)

    const blueprint = blueprints[idx].data?.blueprint
    const puzzle = puzzles[idx].data

    if (puzzles[idx].isLoading || blueprints[idx].isLoading) {
      return <LoadingSpinner key={key} />
    }

    // This would be annoying to useCallback so I'm skipping it
    const handleClick = () => {
      onSelectLocation(xt, yt)
    }

    if (oob) {
      return <OOBTile key={key} />
    } else if (!puzzle) {
      return <LoadingSpinner key={key} />
    } else if (!loc.blueprint) {
      return (
        <EmptyTile
          key={key}
          puzzle={puzzle}
          toModCount={loc.toModCount}
          onClick={handleClick}
        />
      )
    } else if (xt === viewXt && yt === viewYt) {
      return (
        <div key={key} onClick={handleClick} css={tileWrapperStyles}>
          {selectedBlueprint ? (
            <ModMachineTileView
              puzzle={puzzle}
              blueprint={blueprint}
              width={TILE_WIDTH}
              height={TILE_WIDTH}
              tileWidth={modMachine.tile_size.x}
              tileHeight={modMachine.tile_size.y}
            />
          ) : null}
        </div>
      )
    } else {
      return (
        <div key={key} css={tileWrapperStyles} onClick={handleClick}>
          <ModMachineTileView
            puzzle={puzzle}
            blueprint={blueprint}
            width={TILE_WIDTH}
            height={TILE_WIDTH}
            tileWidth={modMachine.tile_size.x}
            tileHeight={modMachine.tile_size.y}
          />
        </div>
      )
    }
  })

  return (
    <div
      css={{
        display: 'grid',
        gridTemplateColumns: `repeat(${sideSize}, ${TILE_WIDTH}px)`,
        gridTemplateRows: `repeat(${sideSize}, ${TILE_WIDTH}px)`,
        gridColumnGap: '0px',
        gridRowGap: '0px',
        gridAutoFlow: 'row',
        aspectRatio: '1',
        justifyItems: 'center',
        alignItems: 'center',
        width: sideSize * TILE_WIDTH,
      }}
    >
      {contextTiles}
    </div>
  )
}
