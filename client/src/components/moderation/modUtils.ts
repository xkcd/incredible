import { random, sample, sortBy } from 'lodash'
import { gridDimensions, iterTiles } from '../../lib/tiles'
import { intersectBounds } from '../../lib/utils'
import { WidgetCollection } from '../../types'
import { interestingWeights } from './interestingWeights'
import {
  CandidateMap,
  ModLocation,
  ModMachine,
  ServerBlueprint,
} from './modTypes'

export function getRandEmptyNearTile(
  modMachine: ModMachine,
  baseXt: number,
  baseYt: number,
  contextWindow: number,
): ModLocation | undefined {
  const [tilesX, tilesY] = gridDimensions(modMachine.grid)
  const nearbyEmpties: ModLocation[] = []
  for (const [xt, yt] of iterTiles(
    ...intersectBounds(
      [baseXt, baseYt, baseXt + contextWindow, baseYt + contextWindow],
      [0, 0, tilesX - 1, tilesY - 1],
    ),
  )) {
    const tile = modMachine.grid[yt][xt]
    if (!tile.blueprint && tile.to_mod) {
      nearbyEmpties.push({ ...modMachine.grid[yt][xt], xt, yt })
    }
  }
  if (nearbyEmpties.length > 0) {
    return sample(nearbyEmpties)
  }
  return undefined
}

export function getEmptyTile(modMachine: ModMachine): ModLocation {
  const [tilesX, tilesY] = gridDimensions(modMachine.grid)
  const emptyTiles: ModLocation[] = []
  for (const [xt, yt] of iterTiles(0, 0, tilesX - 1, tilesY - 1)) {
    const { blueprint } = modMachine.grid[yt][xt]
    if (blueprint) {
      const nearbyEmpty = getRandEmptyNearTile(modMachine, xt, yt, 5)
      if (nearbyEmpty) {
        return nearbyEmpty
      }
    } else {
      emptyTiles.push({ ...modMachine.grid[yt][xt], xt, yt })
    }
  }
  if (emptyTiles.length > 0) {
    return emptyTiles[random(emptyTiles.length)]
  }
  const xt = random(tilesX)
  const yt = random(tilesY)
  return { ...modMachine.grid[yt][xt], xt, yt }
}

export function calculateInterest(b: ServerBlueprint): number {
  const widgets = b.widgets as WidgetCollection
  let interestingness: number = 0
  for (const [_, widget] of Object.entries(widgets)) {
    interestingness += interestingWeights[widget.type]
  }
  return interestingness
}

export function sortCandidateMap(
  candidates: CandidateMap,
  currentBlueprintId: string | undefined,
): Array<[string, ServerBlueprint]> {
  return sortBy([...candidates.entries()], ([blueprintId, blueprint]) =>
    blueprintId === currentBlueprintId
      ? -Infinity
      : -calculateInterest(blueprint),
  )
}

export function locFromPosition(
  grid: ModMachine['grid'],
  xt: number,
  yt: number,
) {
  return {
    ...grid[yt][xt],
    xt: xt,
    yt: yt,
  }
}
