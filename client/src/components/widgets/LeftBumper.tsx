import imgBonkOff from '@art/tribonker-off_4x.png'
import imgBonkOn from '@art/tribonker-on_4x.png'
import { useState } from 'react'
import { coords, vectorScale } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage, ComicImageAnimation } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider, useCollisionHandler } from '../PhysicsContext'
import {
  BONK_ANIMATION_DELAY_MS,
  TRIANGLE_BUMPER_CONTACT_DISTANCE,
  TRIANGLE_BUMPER_RADIUS_RATIO,
  TRIANGLE_BUMPER_SENSOR_FUDGE,
  TRIANGLE_BUMPER_SENSOR_OFFSET,
  TRIANGLE_BUMPER_STRENGTH,
} from '../constants'
import { getPositionStyles } from '../positionStyles'

export interface LeftBumperWidget extends Vector, Angled {
  type: 'leftbumper'
}

const imgs = [imgBonkOff, imgBonkOn]

export function LeftBumperPreview() {
  return <ComicImage img={imgBonkOff} css={{ width: '50%', height: 'auto' }} />
}

export default function LeftBumper({
  id,
  onSelect,
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  isSelected,
  x,
  y,
  angle,
}: LeftBumperWidget & EditableWidget) {
  const width = imgBonkOff.width
  const height = imgBonkOff.height
  const triangleRadius = width * TRIANGLE_BUMPER_RADIUS_RATIO
  const sensorOffsetRadius = triangleRadius * TRIANGLE_BUMPER_SENSOR_OFFSET

  const bodyRef = useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setRotation(coords.toRapier.angle(angle))
          .setCcdEnabled(true),
        colliderDescs: [
          // Body
          ColliderDesc.roundTriangle(
            coords.toRapier.vectorObject(
              0.5 * width - triangleRadius,
              -0.5 * height + triangleRadius,
            ),
            coords.toRapier.vectorObject(
              -0.5 * width + triangleRadius,
              0.5 * height - triangleRadius,
            ),
            coords.toRapier.vectorObject(
              0.5 * width - triangleRadius,
              0.5 * height - triangleRadius,
            ),
            coords.toRapier.length(triangleRadius),
          ).setRestitution(0.1),
        ],
      }
    },
    [angle, triangleRadius, height, width, x, y],
  )

  const bumperCollider = useCollider(
    ({ ColliderDesc, ActiveEvents, ActiveCollisionTypes }) =>
      ColliderDesc.roundConvexPolyline(
        new Float32Array([
          ...coords.toRapier.vector(
            0.5 * width - triangleRadius * TRIANGLE_BUMPER_SENSOR_FUDGE,
            -0.5 * height + triangleRadius,
          ),
          ...coords.toRapier.vector(
            -0.5 * width,
            0.5 * height - triangleRadius,
          ),
        ]),
        coords.toRapier.length(sensorOffsetRadius),
      )!
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRotation(coords.toRapier.angle(angle))
        .setSensor(true)
        .setActiveEvents(
          ActiveEvents.COLLISION_EVENTS | ActiveEvents.CONTACT_FORCE_EVENTS,
        )
        .setActiveCollisionTypes(ActiveCollisionTypes.ALL),
    [angle, triangleRadius, sensorOffsetRadius, width, height, x, y],
  )

  const strength = TRIANGLE_BUMPER_STRENGTH
  const bonkOnMS = BONK_ANIMATION_DELAY_MS

  const [isContacting, setContacting] = useState(0)

  useCollisionHandler(
    'start',
    bumperCollider,
    (otherCollider) => {
      const otherBody = otherCollider.parent()
      if (!bumperCollider || !otherBody) {
        return
      }

      const contact = bumperCollider.contactCollider(
        otherCollider,
        TRIANGLE_BUMPER_CONTACT_DISTANCE,
      )

      if (contact == null) {
        return
      }

      const forceVector = vectorScale(
        contact.normal1,
        strength / Math.max(otherBody.invMass(), Number.MIN_VALUE),
      )

      otherBody.applyImpulseAtPoint(forceVector, contact.point2, true)
      setContacting( ( isContacting ) => isContacting + 1)
      setTimeout(() => setContacting(( isContacting ) => Math.max(isContacting - 1, 0)), bonkOnMS)
    },
    [bodyRef, strength, isContacting, bonkOnMS],
  )

  return (
    <ComicImageAnimation
      {...useSelectHandlers(id, onSelect)}
      imgs={imgs}
      showIdx={isContacting > 0 ? 1 : 0}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
