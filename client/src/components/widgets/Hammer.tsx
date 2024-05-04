import imgHammer from '@art/hammer_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

export interface HammerWidget extends Vector, Angled {
  type: 'hammer'
}

export function HammerPreview() {
  return (
    <ComicImage
      img={imgHammer}
      css={{ width: '100%', height: 'auto', transform: 'rotate(-45deg)' }}
    />
  )
}

export function Hammer({
  id,
  onSelect,
  x,
  y,
  angle,
}: HammerWidget & EditableWidget) {
  const width = imgHammer.width
  const height = imgHammer.height
  const headWidth = 23
  const shaftThickness = 10
  const handleLength = 30
  const handleThickness = 12
  const handleRadius = 2

  useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // Shaft
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(width / 2, shaftThickness / 2),
          ).setRestitution(0.7),

          // Handle
          ColliderDesc.roundCuboid(
            ...coords.toRapier.lengths(
              handleLength / 2 - handleRadius,
              handleThickness / 2 - handleRadius,
              handleRadius,
            ),
          )
            .setTranslation(
              ...coords.toRapier.vector(-width / 2 + handleLength / 2 + 4, 0),
            )
            .setRestitution(0.7),

          // Head
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(headWidth / 2, height / 2),
          )
            .setTranslation(
              ...coords.toRapier.vector(width / 2 - headWidth / 2, 0),
            )
            .setRestitution(0.8),
        ],
      }
    },
    [angle, height, width, x, y],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgHammer}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
