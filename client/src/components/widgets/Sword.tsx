import imgSword from '@art/sword_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

export interface SwordWidget extends Vector, Angled {
  type: 'sword'
}

export function SwordPreview() {
  return (
    <ComicImage
      img={imgSword}
      css={{ width: '100%', height: 'auto', transform: 'rotate(-45deg)' }}
    />
  )
}

export function Sword({
  id,
  onSelect,
  x,
  y,
  angle,
}: SwordWidget & EditableWidget) {
  const width = imgSword.width
  const height = imgSword.height
  const guardWidth = 9
  const guardOffset = 35
  const handleThickness = 10
  const tipLength = handleThickness

  useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // Handle
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(
              width / 2 - tipLength / 2,
              handleThickness / 2,
            ),
          )
            .setTranslation(...coords.toRapier.vector(-tipLength, 0))
            .setRestitution(0.9),

          // Tip
          ColliderDesc.triangle(
            coords.toRapier.vectorObject(
              width / 2 - tipLength,
              -handleThickness / 2,
            ),
            coords.toRapier.vectorObject(width / 2, 0),
            coords.toRapier.vectorObject(
              width / 2 - tipLength,
              handleThickness / 2,
            ),
          ).setRestitution(0.9),

          // Guard
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(guardWidth / 2, height / 2),
          )
            .setTranslation(...coords.toRapier.vector(-guardOffset, 0))
            .setRestitution(0.9),
        ],
      }
    },
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgSword}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
