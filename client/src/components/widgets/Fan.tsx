import img1 from '@art/fan-blade1_4x.png'
import img2 from '@art/fan-blade2_4x.png'
import img3 from '@art/fan-blade3_4x.png'
import img4 from '@art/fan-blade4_4x.png'
import { coords, vectorDistance } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage, ComicImageAnimation } from '../ComicImage'
import { useRigidBody, useSensorInTile } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { TICK_MS, useCollider, usePhysicsLoaded } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'

export interface FanWidget extends Vector, Angled {
  type: 'fan'
}

const imgs = [img1, img2, img3, img4]

export function FanPreview() {
  return <ComicImage img={img1} css={{ width: '50%', height: 'auto' }} />
}

export default function Fan({
  id,
  onSelect,
  isSelected,
  x,
  y,
  angle,
}: FanWidget & EditableWidget) {
  const width = img1.width
  const height = img1.height
  const bladesWidth = width / 5
  const bodyHeight = height / 3
  const airOffset = 20
  const radius = 10
  const length = 300
  const strength = 0.05

  const bodyRef = useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // Body
          ColliderDesc.roundCuboid(
            ...coords.toRapier.lengths(
              width / 2 - radius,
              bodyHeight / 2 - radius,
            ),
            coords.toRapier.length(radius),
          ).setRestitution(0.5),

          // Blades
          ColliderDesc.roundCuboid(
            ...coords.toRapier.lengths(
              bladesWidth / 2 - radius,
              height / 2 - radius,
            ),
            coords.toRapier.length(radius),
          )
            .setTranslation(
              ...coords.toRapier.vector(width / 2 - bladesWidth / 2, 0),
            )
            .setRestitution(2.5),
        ],
      }
    },
    [angle, bladesWidth, bodyHeight, height, width, x, y],
  )

  const airCollider = useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.cuboid(
        ...coords.toRapier.lengths((length - airOffset) / 2, height / 2),
      )
        .setTranslation(
          ...coords.toRapier.vector(
            x + 0.5 * length * Math.cos(angle),
            y + 0.5 * length * Math.sin(angle),
          ),
        )
        .setRotation(coords.toRapier.angle(angle))
        .setSensor(true),
    [angle, height, x, y],
  )

  const xComponent = Math.cos(coords.toRapier.angle(angle))
  const yComponent = Math.sin(coords.toRapier.angle(angle))
  const falloffDistance = coords.toRapier.length(length)

  useSensorInTile(
    airCollider,
    (otherCollider) => {
      const { current: body } = bodyRef
      const otherBody = otherCollider.parent()
      if (!body || !otherBody) {
        return
      }

      const distance = vectorDistance(
        otherBody.translation(),
        body.translation(),
      )

      const falloff = Math.max(
        0,
        Math.pow((falloffDistance - distance) / falloffDistance, 2),
      )

      const forceVector = {
        x: strength * falloff * xComponent,
        y: strength * falloff * yComponent,
      }

      otherBody.applyImpulse(forceVector, true)
    },
    [bodyRef, falloffDistance, xComponent, yComponent],
  )

  const hasPhysics = usePhysicsLoaded()

  return (
    <div
      {...useSelectHandlers(id, onSelect)}
      css={{
        width,
        height,
      }}
      style={getPositionStyles(x, y, angle)}
    >
      <ComicImageAnimation
        css={{
          position: 'absolute',
        }}
        imgs={imgs}
        rateMs={hasPhysics ? TICK_MS : 0}
      />
      {isSelected && (
        <div
          css={{
            position: 'absolute',
            background:
              'linear-gradient(to right, rgba(0, 0, 0, .5), transparent)',
            pointerEvents: 'none',
            width: length,
            height,
            marginLeft: airOffset * 2,
            opacity: 0.25,
          }}
        />
      )}
    </div>
  )
}
