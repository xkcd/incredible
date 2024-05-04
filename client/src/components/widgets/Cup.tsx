import imgCup from '@art/cup_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

export interface CupWidget extends Vector, Angled {
  type: 'cup'
}

export function CupPreview() {
  return <ComicImage img={imgCup} css={{ width: '50%', height: 'auto' }} />
}

export function Cup({ id, onSelect, x, y, angle }: CupWidget & EditableWidget) {
  const width = imgCup.width
  const height = imgCup.height

  useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // Bottom
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(width / 3, height / 12),
          )
            .setTranslation(
              ...coords.toRapier.vector(0, height / 2 - height / 12),
            )
            .setRestitution(0.9),

          // Left
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(width / 16, height / 2 - 2),
          )
            .setTranslation(...coords.toRapier.vector(-width / 3, 0))
            .setRotation(coords.toRapier.angle(-Math.PI / 16))
            .setRestitution(0.9),

          // Right
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(width / 16, height / 2 - 2),
          )
            .setTranslation(...coords.toRapier.vector(width / 3, 0))
            .setRotation(coords.toRapier.angle(Math.PI / 16))
            .setRestitution(0.9),
        ],
      }
    },
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgCup}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
