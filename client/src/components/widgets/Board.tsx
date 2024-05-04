import imgBoard from '@art/board_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'

export interface BoardWidget extends Vector, Angled {
  type: 'board'
}

export function BoardPreview() {
  return (
    <ComicImage
      img={imgBoard}
      css={{ width: '100%', height: 'auto', transform: 'rotate(-45deg)' }}
    />
  )
}

export function Board({
  id,
  onSelect,
  x,
  y,
  angle,
}: BoardWidget & EditableWidget) {
  const width = imgBoard.width
  const height = imgBoard.height

  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(width / 2, height / 2))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRotation(coords.toRapier.angle(angle))
        .setRestitution(0.5),
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgBoard}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
