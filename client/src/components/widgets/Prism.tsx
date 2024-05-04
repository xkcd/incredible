import { useCallback } from 'react'
import { coords } from '../../lib/coords'
import { Angled, Ball, Vector, isBall } from '../../types'
import { ComicImage } from '../ComicImage'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

import imgPrism from '@art/prism_4x.png'
import type { Collider, RigidBody } from '@dimforge/rapier2d'
import { Basis } from '../../lib/utils'
import { useCollider, useCollisionHandler } from '../PhysicsContext'

export interface PrismWidget extends Vector, Angled {
  type: 'prism'
}

export function PrismPreview() {
  return <ComicImage img={imgPrism} css={{ width: '80%', height: 'auto' }} />
}

export function pointToVectorObject(
  { xBasis, yBasis }: Basis,
  x: number,
  y: number,
) {
  return coords.toRapier.vectorObject(x - xBasis, y - yBasis)
}

const width = imgPrism.width
const height = imgPrism.height
const MIN_CONTACT_DISTANCE = 0.1
const PRISM_REFRACTION_INDEX = 2.0

function setRefractionVector(
  // 2:1 <-> 1:2 is fine, but fyi btw 80:1 <-> 1:80 is really really not - it gets weird as you go towards 0
  indexRatio: (ball: Ball) => number,
  // new speed is equal to old speed multiplied by this
  speedMult: (ball: Ball) => number,
  // prism
  prismCollider: Collider | undefined,
  otherCollider: Collider,
  // are you joining the prism party? or leaving?
  headingIn: boolean,
  // stuff prints when this is non-null
  logString?: string,
) {
  const ball: RigidBody | null = otherCollider.parent()
  if (ball == null || !isBall(ball) || prismCollider == null) {
    return null
  }

  const contact = prismCollider.contactCollider(
    otherCollider,
    MIN_CONTACT_DISTANCE,
  )
  if (contact == null) {
    return null
  }

  // I did a lot of holding up two pencils U_U
  // normal1 points towards the 'outside' of the prism, and normal2 points inside
  // so when you're coming into the prism you want normal1 and when you're going out you want normal2
  // (or the reverse and then you don't invert the velocity vector)
  const normal = headingIn ? contact.normal1 : contact.normal2
  const invNormal = headingIn ? contact.normal2 : contact.normal1
  const ballVellocity = ball.linvel()
  const ballMass = ball.mass()

  const surfaceAngle = Math.atan2(normal.y, normal.x)
  // please make sure all vectors are fastened into their seatbelts and pointed in the same direction as the normals
  const velocityAngle = Math.atan2(
    -1.0 * ballVellocity.y,
    -1.0 * ballVellocity.x,
  )
  // convert to surface normal coordinate system
  const angleInc = velocityAngle - surfaceAngle
  // sin(incident) / sin(refraction) = index destination / index start
  const angleRefr = Math.asin(
    // clamp to the domain of asin *just in case*
    Math.max(Math.min(Math.sin(angleInc) / indexRatio(ball), 1), -1),
  )

  if (logString) {
    console.log(`** ${logString}
        surfaceNormal: ${contact.normal1.x.toFixed(3)}, ${contact.normal1.y.toFixed(3)}
        surfaceNormal2: ${contact.normal2.x.toFixed(3)}, ${contact.normal2.y.toFixed(3)}
        prismRot: ${(prismCollider.rotation() / Math.PI).toFixed(3)}
        surfNorm: x:${normal.x.toFixed(3)}, y:${normal.y.toFixed(3)}
        surfAngle/pi: ${(surfaceAngle / Math.PI).toFixed(3)}
        velocityAngle/pi: ${(velocityAngle / Math.PI).toFixed(3)}
        ballType: ${ball.userData.ballType}
        ballMass: ${ballMass}
        angleInc/pi: ${(angleInc / Math.PI).toFixed(3)}
        angleRefr/pi: ${(angleRefr / Math.PI).toFixed(3)}
        ballVelocity: ${ballVellocity.x.toFixed(3)}, ${ballVellocity.y.toFixed(3)}
        indexRatio: ${indexRatio(ball)}
      `)
  }

  const newX =
    invNormal.x * Math.cos(angleRefr) - invNormal.y * Math.sin(angleRefr)
  const newY =
    invNormal.x * Math.sin(angleRefr) + invNormal.y * Math.cos(angleRefr)

  const speed =
    Math.sqrt(ballVellocity.x ** 2 + ballVellocity.y ** 2) * speedMult(ball)

  if (logString) {
    console.log(`<<${logString}
          new normal: x:${newX.toFixed(3)}, y:${newY.toFixed(3)}
    >>`)
  }
  ball.setLinvel({ x: newX * speed, y: newY * speed }, true)
}

export default function Prism({
  id,
  onSelect,
  x,
  y,
  angle,
}: PrismWidget & EditableWidget) {
  const collider = useCollider(
    ({ ColliderDesc, ActiveEvents }) => {
      const vector = pointToVectorObject.bind(undefined, {
        xBasis: width / 2.0,
        yBasis: height / 2.0,
      })
      return ColliderDesc.triangle(
        vector(0, height),
        vector(width, height),
        vector(width / 2.0, 0),
      )
        .setSensor(true)
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRotation(coords.toRapier.angle(angle))
        .setActiveEvents(
          ActiveEvents.COLLISION_EVENTS | ActiveEvents.CONTACT_FORCE_EVENTS,
        )
    },
    [x, y, angle],
  )

  const collisionStart = useCallback(
    (otherCollider: Collider) => {
      setRefractionVector(
        // TODO: base on ball properties
        (_) => PRISM_REFRACTION_INDEX,
        // TODO: idk if we want to be able to speed up or slow down the balls, but it is possible here
        (_) => 1.0,
        collider,
        otherCollider,
        true,
      )
    },
    [collider],
  )

  const collisionEnd = useCallback(
    (otherCollider: Collider) => {
      setRefractionVector(
        // TODO: see above
        (_) => 1.0 / PRISM_REFRACTION_INDEX,
        (_) => 1.0,
        collider,
        otherCollider,
        false,
      )
    },
    [collider],
  )

  useCollisionHandler('start', collider, collisionStart, [])
  useCollisionHandler('end', collider, collisionEnd, [])

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgPrism}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
