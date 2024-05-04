import { Puzzle, WidgetCollection } from '../../types'

export const emptyPuzzle: Puzzle = {
  inputs: [
    { x: 0.5, y: 0, balls: [{ type: 1, rate: 1 }] },
    { x: 0, y: 0.5, balls: [{ type: 2, rate: 1 }] },
    { x: 1, y: 0.75, balls: [{ type: 3, rate: 1 }] },
    { x: 1, y: 0.25, balls: [{ type: 4, rate: 1 }] },
  ],
  outputs: [
    { x: 0.5, y: 1, balls: [{ type: 1, rate: 1 }] },
    { x: 1, y: 0.5, balls: [{ type: 2, rate: 1 }] },
    { x: 0, y: 0.75, balls: [{ type: 3, rate: 1 }] },
    { x: 0, y: 0.25, balls: [{ type: 4, rate: 1 }] },
  ],
}

export const emptyWidgets: WidgetCollection = {}
