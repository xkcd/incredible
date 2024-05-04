import boatImage from '@art/boat_4x.png'
import { type Vector } from '@dimforge/rapier2d'
import { coords } from '../../lib/coords'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget } from '../MachineTileEditor'
import { useLoopHandler, useRapierEffect } from '../PhysicsContext'
import { getPositionStyles, usePositionedBodyRef } from '../positionStyles'

export default function Boat({ id, x, y }: Vector & EditableWidget) {
  const radius = 30
  const bodyRef = useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: id,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Dynamic)
          .setLinearDamping(6)
          .setAngularDamping(2),
        colliderDescs: [
          // Weight
          ColliderDesc.capsule(
            coords.toRapier.length(boatImage.width / 3 - radius),
            coords.toRapier.length(radius),
          )
            .setTranslation(...coords.toRapier.vector(30, 45))
            .setRotation(Math.PI / 2)
            .setMass(12),

          // Sensor
          ColliderDesc.cuboid(
            ...coords.toRapier.lengths(
              boatImage.width / 3,
              boatImage.height / 2,
            ),
          )
            .setTranslation(...coords.toRapier.vector(0, 10))
            .setSensor(true),
        ],
      }
    },
    [id],
  )

  useRapierEffect(() => {
    const { current: body } = bodyRef
    if (!body) {
      return
    }

    body.setTranslation(coords.toRapier.vectorObject(x, y), true)
  }, [bodyRef, x, y])

  useLoopHandler(
    ({ world }) => {
      const { current: body } = bodyRef
      if (!body) {
        return
      }

      const rotation = body.rotation()
      const springConstant = -1
      body.resetTorques(true)
      body.applyTorqueImpulse(rotation * springConstant, true)

      let countTouchingWater = 0
      world.intersectionPairsWith(body.collider(1), () => {
        countTouchingWater++
      })

      if (countTouchingWater) {
        body.applyImpulse(
          { x: 0, y: coords.toRapier.y(-6 * countTouchingWater) },
          true,
        )
      }
    },
    [bodyRef],
  )

  return (
    <ComicImage
      ref={usePositionedBodyRef(bodyRef, {
        width: boatImage.width,
        height: boatImage.height,
        initialX: x,
        initialY: y,
      })}
      css={{ zIndex: 20 }}
      style={getPositionStyles(x, y)}
      img={boatImage}
    ></ComicImage>
  )
}
