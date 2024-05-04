import imgBallStand from '@art/ball-stand_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

export interface BallStandWidget extends Vector, Angled {
  type: 'ballstand'
}

export function BallStandPreview() {
  return (
    <ComicImage img={imgBallStand} css={{ width: 'auto', height: '80%' }} />
  )
}

export function BallStand({
  id,
  onSelect,
  x,
  y,
  angle,
}: BallStandWidget & EditableWidget) {
  const width = imgBallStand.width
  const height = imgBallStand.height

  useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // Stand
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(width / 4, height / 2 - 6),
          )
            .setTranslation(...coords.toRapier.vector(0, 4))
            .setRestitution(0.75),

          // Base
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(width / 2, height / 12),
          )
            .setTranslation(
              ...coords.toRapier.vector(0, height / 2 - height / 12),
            )
            .setRestitution(0.75),

          // Top left
          ColliderDesc.triangle(
            coords.toRapier.vectorObject(width / 5, -height / 2),
            coords.toRapier.vectorObject(0, -height / 2 + 10),
            coords.toRapier.vectorObject(0, -height / 2 + 4),
          ).setRestitution(0.1),

          // Top right
          ColliderDesc.triangle(
            coords.toRapier.vectorObject(-width / 5, -height / 2),
            coords.toRapier.vectorObject(0, -height / 2 + 10),
            coords.toRapier.vectorObject(0, -height / 2 + 4),
          ).setRestitution(0.1),
        ],
      }
    },
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgBallStand}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
