import imgBonkOff from '@art/bonker-off_4x.png'
import imgBonkOn from '@art/bonker-on_4x.png'
import { useState } from 'react'
import {
  coords,
  vectorDifference,
  vectorNorm,
  vectorScale,
} from '../../lib/coords'
import { Vector, isBall } from '../../types'
import { ComicImage, ComicImageAnimation } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider, useCollisionHandler } from '../PhysicsContext'
import {
  BONK_ANIMATION_DELAY_MS,
  TRIANGLE_BUMPER_CONTACT_DISTANCE as BUMPER_CONTACT_DISTANCE,
  ROAND_BUMPER_RADIUS_RATIO,
  ROUND_BUMPER_FALLBACK_RATIO,
  ROUND_BUMPER_STRENGTH,
} from '../constants'
import { getPositionStyles } from '../positionStyles'

export interface RoundBumperWidget extends Vector {
  type: 'roundbumper'
}

const imgs = [imgBonkOff, imgBonkOn]
const strength = ROUND_BUMPER_STRENGTH
const bonkOnMs = BONK_ANIMATION_DELAY_MS

export function RoundBumperPreview() {
  return <ComicImage img={imgBonkOff} css={{ width: '65%', height: 'auto' }} />
}

export default function RoundBumper({
  id,
  onSelect,
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  isSelected,
  x,
  y,
}: RoundBumperWidget & EditableWidget) {
  const width = imgBonkOff.width
  const bumperRadius = width * ROAND_BUMPER_RADIUS_RATIO

  const bodyRef = useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setCcdEnabled(true),
        colliderDescs: [
          ColliderDesc.capsule(
            0,
            coords.toRapier.length(bumperRadius * ROUND_BUMPER_FALLBACK_RATIO),
          ).setRestitution(1),
        ],
      }
    },
    [bumperRadius, x, y],
  )

  const bumperCollider = useCollider(
    ({ ColliderDesc, ActiveEvents, ActiveCollisionTypes }) =>
      ColliderDesc.capsule(0, coords.toRapier.length(bumperRadius))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setSensor(true)
        .setActiveEvents(
          ActiveEvents.COLLISION_EVENTS | ActiveEvents.CONTACT_FORCE_EVENTS,
        )
        .setActiveCollisionTypes(ActiveCollisionTypes.ALL),
    [bumperRadius, x, y],
  )

  const [isContacting, setContacting] = useState(0)

  useCollisionHandler(
    'start',
    bumperCollider,
    (otherCollider) => {
      const { current: body } = bodyRef
      if (!bumperCollider || !body) {
        return
      }
      const otherBody = otherCollider.parent()
      if (!otherBody) {
        return
      }

      const contact = bumperCollider.contactCollider(
        otherCollider,
        BUMPER_CONTACT_DISTANCE,
      )

      if (contact !== null) {
        const bodyIsBall = isBall(otherBody)

        otherBody.applyImpulseAtPoint(
          vectorScale(
            vectorNorm(
              vectorDifference(
                otherCollider.translation(),
                bumperCollider.translation(),
              ),
            ),
            strength /
              Math.max(
                bodyIsBall ? otherBody.invMass() : otherBody.mass(),
                Number.MIN_VALUE,
              ),
          ),
          contact?.point2,
          true,
        )
        setContacting((isContacting) => isContacting + 1)
        setTimeout(
          () => setContacting((isContacting) => Math.max(isContacting - 1, 0)),
          bonkOnMs,
        )
      }
    },
    [bodyRef, strength, isContacting, bonkOnMs],
  )

  return (
    <ComicImageAnimation
      {...useSelectHandlers(id, onSelect)}
      imgs={imgs}
      showIdx={isContacting > 0 ? 1 : 0}
      style={getPositionStyles(x, y, 0)}
    />
  )
}
