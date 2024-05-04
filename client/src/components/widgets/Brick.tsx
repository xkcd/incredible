import imgBrick from '@art/brick_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'

export interface BrickWidget extends Vector, Angled {
  type: 'brick'
}

export function BrickPreview() {
  return <ComicImage img={imgBrick} css={{ width: '80%', height: 'auto' }} />
}

export function Brick({
  id,
  onSelect,
  x,
  y,
  angle,
}: BrickWidget & EditableWidget) {
  const width = imgBrick.width
  const height = imgBrick.height

  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(width / 2, height / 2))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRotation(coords.toRapier.angle(angle))
        .setRestitution(0.75),
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgBrick}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
