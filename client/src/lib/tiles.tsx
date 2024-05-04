import { Bounds } from '../types'
import { intersectBounds } from './utils'

export type Grid<T> = T[][]

export function tileBounds(
  x1: number,
  y1: number,
  x2: number,
  y2: number,
  tileWidth: number,
  tileHeight: number,
  outset: number = 0,
): Bounds {
  const xt1 = Math.floor((x1 - outset) / tileWidth)
  const yt1 = Math.floor((y1 - outset) / tileHeight)

  // The bottom right bounds are -1 tile to factor in the last tile's width and height.
  const xt2 = Math.ceil((x2 + outset) / tileWidth) - 1
  const yt2 = Math.ceil((y2 + outset) / tileHeight) - 1

  return [xt1, yt1, xt2, yt2]
}

export function* iterTiles(
  xt1: number,
  yt1: number,
  xt2: number,
  yt2: number,
): Generator<[xt: number, yt: number]> {
  for (let yt = yt1; yt <= yt2; yt++) {
    for (let xt = xt1; xt <= xt2; xt++) {
      yield [xt, yt]
    }
  }
}

export function gridDimensions(grid: Grid<unknown>) {
  const tilesX = grid[0].length
  const tilesY = grid.length
  return [tilesX, tilesY]
}

export function gridViewBounds(
  viewBounds: Bounds,
  tilesX: number,
  tilesY: number,
  tileWidth: number,
  tileHeight: number,
  outset: number,
): Bounds {
  const rawBounds = tileBounds(...viewBounds, tileWidth, tileHeight, outset)
  return intersectBounds(rawBounds, [0, 0, tilesX - 1, tilesY - 1])
}

export function tileKey(xt: number, yt: number) {
  return `${xt},${yt}`
}
