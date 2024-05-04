import { coords } from '../../lib/coords'
import { Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useRapierEffect } from '../PhysicsContext'
import { usePositionedBodyRef } from '../positionStyles'

import imgWheel from '@art/spoked-wheel_4x.png'
import type { ColliderDesc } from '@dimforge/rapier2d'
import { useRef } from 'react'
import { rotate } from '../../lib/utils'
import { pointToVectorObject } from './Prism'

export interface SpokedWheelWidget extends Vector {
  type: 'spokedwheel'
  speed: number
}

export function SpokedWheelPreview() {
  return <ComicImage img={imgWheel} css={{ width: '85%', height: 'auto' }} />
}

export function getNextWheelSpeed(wheel: SpokedWheelWidget, key: string) {
  const currentSpeed = wheel.speed
  if (
    key === 'arrowleft' ||
    (key === 'arrowdown' && currentSpeed < 0) ||
    (key === 'arrowup' && currentSpeed > 0)
  ) {
    return currentSpeed + 1
  } else if (
    key === 'arrowright' ||
    (key === 'arrowup' && currentSpeed < 0) ||
    (key === 'arrowdown' && currentSpeed > 0)
  ) {
    return currentSpeed - 1
  } else {
    return null
  }
}

// I think this is a really ugly bag of constants and object descriptions
const OUTER_WHEEL_RADIUS = 40.0
const WHEEL_TOP = 38
const WHEEL_LEFT = 79
const NUM_SPOKES = 7

const spokeCapsule = {
  boundRect: { width: 9.0, height: 10.0 },
  distanceFromTop: 12.0,
}
const upperConvexPiece = {
  x: 76,
  y: 18,
  topWidth: 7,
  bottomWidth: 4.5,
  height: 11.0,
}
// typing it liuke this is pretty dumb
type Description<T> = {
  readonly [P in keyof T]: T[P]
}
type HullDescription = Description<typeof upperConvexPiece>
const bottomConvexPiece: HullDescription = {
  x: 77,
  y: 28,
  topWidth: 4.5,
  bottomWidth: 8,
  height: 10,
}

/*
 * the spokes are made from a capsule (the top) and then two convex hulls
 * looking something like this:
 *
 *  ^
 * | |
 *  v
 * \ /
 * / \
 *
 */
function makeConvexHull(hullDesc: HullDescription, rotationAngle: number) {
  const { x, y, topWidth, bottomWidth, height } = hullDesc
  rotate.bind(undefined, rotationAngle)
  const makePoint = (x: number, y: number) =>
    rotate(rotationAngle, pointToVectorObject({ xBasis, yBasis }, x, y))
  const bottomXAdjust = (topWidth - bottomWidth) / 2.0
  return [
    makePoint(x, y),
    makePoint(x + topWidth, y),
    makePoint(x + bottomXAdjust, y + height),
    makePoint(x + bottomXAdjust + bottomWidth, y + height),
  ].flatMap(({ x, y }) => [x, y])
}

function makeSpoke(index: number, colliderDesc: typeof ColliderDesc) {
  const rotationAngle = (index * 2.0 * Math.PI) / NUM_SPOKES
  const translationRotation = rotate(
    rotationAngle,
    coords.toRapier.vectorObject(0, spokeCapsule.distanceFromTop, {
      xBasis: 0,
      yBasis: yBasis,
    }),
  )
  return [
    colliderDesc
      .capsule(
        ...coords.toRapier.lengths(
          spokeCapsule.boundRect.height / 2.0,
          spokeCapsule.boundRect.width / 2.0,
        ),
      )
      .setRotation(rotationAngle)
      .setTranslation(translationRotation.x, translationRotation.y),
    colliderDesc.convexHull(
      new Float32Array(makeConvexHull(upperConvexPiece, rotationAngle)),
    ),
    colliderDesc.convexHull(
      new Float32Array(makeConvexHull(bottomConvexPiece, rotationAngle)),
    ),
  ]
}

const xBasis = imgWheel.width / 2
const yBasis = imgWheel.height / 2

export default function SpokedWheel({
  id,
  onSelect,
  x,
  y,
  isSelected,
  speed,
}: SpokedWheelWidget & EditableWidget) {
  const bodyRef = useRigidBody(
    // @ts-expect-error mad about possibly null convexHull. dont worrty though im filtering null values out
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      const wheelCenter = coords.toRapier.vectorObject(
        WHEEL_LEFT,
        WHEEL_TOP + OUTER_WHEEL_RADIUS,
        {
          xBasis,
          yBasis,
        },
      )
      return {
        key: id,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Dynamic)
          .lockTranslations()
          .setAdditionalMassProperties(2, wheelCenter, 2)
          .setAngularDamping(1),
        colliderDescs: [
          ...Array(NUM_SPOKES)
            .fill(0)
            .flatMap((_, index) => {
              return makeSpoke(index, ColliderDesc)
            })
            .filter((v) => v != null),
          ColliderDesc.ball(
            coords.toRapier.length(OUTER_WHEEL_RADIUS),
          ).setTranslation(wheelCenter.x, wheelCenter.y),
        ],
      }
    },
    [id],
  )

  // Update position without recreating so angular momentum is preserved during edits
  useRapierEffect(() => {
    const { current: body } = bodyRef
    if (!body) {
      return
    }
    body.setTranslation(coords.toRapier.vectorObject(x, y), true)
  }, [bodyRef, x, y])

  const selectedRef = useRef(isSelected)
  selectedRef.current = isSelected
  // separate effect for setting speed so we're not resetting torque when we translate
  useRapierEffect(() => {
    const { current: body } = bodyRef
    if (!body) {
      return
    }
    body.resetTorques(true)
    body.addTorque(speed, true)
  }, [bodyRef, speed])

  return (
    <ComicImage
      ref={usePositionedBodyRef(bodyRef, {
        width: imgWheel.width,
        height: imgWheel.height,
        initialX: x,
        initialY: y,
      })}
      {...useSelectHandlers(id, onSelect)}
      img={imgWheel}
    />
  )
}
