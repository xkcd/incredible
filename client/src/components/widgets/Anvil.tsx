import imgAnvil from '@art/anvil_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

export interface AnvilWidget extends Vector, Angled {
  type: 'anvil'
}

export function AnvilPreview() {
  return <ComicImage img={imgAnvil} css={{ width: '70%', height: 'auto' }} />
}

export function Anvil({
  id,
  onSelect,
  x,
  y,
  angle,
}: AnvilWidget & EditableWidget) {
  const width = imgAnvil.width
  const height = imgAnvil.height

  useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // Top
          ColliderDesc.triangle(
            coords.toRapier.vectorObject(-width / 2, -height / 2),
            coords.toRapier.vectorObject(width / 2, -height / 2),
            coords.toRapier.vectorObject(0, 0),
          ).setRestitution(0.7),

          // Bottom
          ColliderDesc.triangle(
            coords.toRapier.vectorObject(-23, height / 2),
            coords.toRapier.vectorObject(34, height / 2),
            coords.toRapier.vectorObject(-14, 0),
          ).setRestitution(0.7),

          // Center
          ColliderDesc.cuboid(...coords.toRapier.lengths(11, 10))
            .setTranslation(...coords.toRapier.vector(-6, 0))
            .setRestitution(0.7),
        ],
      }
    },
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgAnvil}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
