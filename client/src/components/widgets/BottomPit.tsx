import imgBottomPit from '@art/bottom_pit_4x.png'
import { coords } from '../../lib/coords'
import { Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useCollider } from '../PhysicsContext'
import {
  BOTTOM_PIT_EDGE_WIDTH,
  BOTTOM_PIT_HEIGHT,
  BOTTOM_PIT_WIDTH,
} from '../constants'
import { getPositionStyles } from '../positionStyles'

export function BottomPit({ x, y }: Vector) {
  const width = BOTTOM_PIT_WIDTH
  const height = BOTTOM_PIT_HEIGHT
  const edgeWidth = BOTTOM_PIT_EDGE_WIDTH
  const bottomHeight = 92

  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(edgeWidth / 2, height / 2))
        .setTranslation(
          ...coords.toRapier.vector(x - width / 2 + edgeWidth / 2, y),
        )
        .setRestitution(0.1),
    [edgeWidth, height, width, x, y],
  )

  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(edgeWidth / 2, height / 2))
        .setTranslation(
          ...coords.toRapier.vector(x + width / 2 - edgeWidth / 2, y),
        )
        .setRestitution(0.1),
    [edgeWidth, height, width, x, y],
  )

  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(
        ...coords.toRapier.lengths(width / 2, bottomHeight / 2),
      )
        .setTranslation(
          ...coords.toRapier.vector(x, y + height / 2 - bottomHeight / 2),
        )
        .setRestitution(0.1),
    [height, width, x, y],
  )

  useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(
        ...coords.toRapier.lengths(width / 2, bottomHeight / 2),
      )
        .setTranslation(
          ...coords.toRapier.vector(x, y + height / 2 - bottomHeight / 2),
        )
        .setRestitution(0.1),
    [height, width, x, y],
  )

  return <ComicImage img={imgBottomPit} style={getPositionStyles(x, y)} />
}
