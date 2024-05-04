import imgCatNoSwat from '@art/cat-noswat_4x.png'
import imgCatSwat from '@art/cat-swat_4x.png'
import type { Collider } from '@dimforge/rapier2d'
import { Dispatch, SetStateAction, useEffect, useRef, useState } from 'react'
import { coords, vectorScale } from '../../lib/coords'
import { RandallPath, rotate } from '../../lib/utils'
import { Angled, Vector, isBall } from '../../types'
import { ComicImage, ComicImageAnimation } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollisionHandler, useRapierEffect } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'
import { pointToVectorObject } from './Prism'
import Ball from './lib/ball'
import { lineCuboid } from './lib/lineCuboid'

export interface CatSwatWidget extends Vector, Angled {
  type: 'catswat'
  catMass: number
}

export function CatSwatPreview() {
  return (
    <div
      css={{
        position: 'relative',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: '80%',
        height: '80%',
        overflow: 'hidden',
      }}
    >
      <ComicImage
        img={imgCatNoSwat}
        css={{
          position: 'absolute',
          width: '100%',
          height: 'auto',
          left: 8,
        }}
      />
    </div>
  )
}

const imgs = [imgCatNoSwat, imgCatSwat]
const CAT_REST_KEY = imgs.indexOf(imgCatNoSwat)
const CAT_BONK_KEY = imgs.indexOf(imgCatSwat)
const MAX_CAT_SWAT_ANGLE = Math.PI / 2.0

const imageScale = 4
const width = imgCatSwat.width * imageScale
const height = imgCatSwat.height * imageScale
const basis = { xBasis: width / 2.0, yBasis: height / 2.0, scale: imageScale }

const CAT_PATH: RandallPath[] = [
  // tail
  { x1: 22, y1: 82, x2: 25, y2: 108, thickness: 2 },
  { x1: 25, y1: 108, x2: 13, y2: 139, thickness: 2 },
  { x1: 13, y1: 139, x2: 18, y2: 165, thickness: 2 },
  { x1: 18, y1: 165, x2: 32, y2: 175, thickness: 2 },
  { x1: 32, y1: 175, x2: 58, y2: 172, thickness: 2 },
  // body
  { x1: 88, y1: 100, x2: 145, y2: 100, thickness: 150 },
  // ears
  { x1: 95, y1: 28, x2: 99, y2: 1, thickness: 1 },
  { x1: 99, y1: 1, x2: 118, y2: 18, thickness: 1 },
  { x1: 118, y1: 18, x2: 138, y2: 18, thickness: 1 },
  { x1: 138, y1: 18, x2: 155, y2: 5, thickness: 1 },
  { x1: 155, y1: 5, x2: 158, y2: 27, thickness: 1 },
]

function performSwat(
  swatCollider: Collider | undefined,
  ballCollider: Collider,
  inBabyJailRef: React.MutableRefObject<NodeJS.Timeout | undefined>,
  inSwipeModeRef: React.MutableRefObject<NodeJS.Timeout | undefined>,
  setImgKey: Dispatch<SetStateAction<number>>,
  catMass: number,
) {
  const ball = ballCollider.parent()
  const cat = swatCollider?.parent()
  const collision = swatCollider?.contactCollider(ballCollider, 0.2)
  if (
    inBabyJailRef.current != null ||
    !ball ||
    !isBall(ball) ||
    !swatCollider ||
    !cat ||
    !collision
  ) {
    return
  }

  // illegal to swat behind
  const catRotation = swatCollider.rotation()
  const impactNormalr = rotate(-catRotation, collision.normal1)
  const impactAngler = Math.atan2(impactNormalr.y, impactNormalr.x)
  if (Math.abs(impactAngler) > MAX_CAT_SWAT_ANGLE) {
    return
  }

  // if want swat and not swat then start swat
  //   keep swat 300ms
  //   chill in baby jail for 200ms
  //   leave baby jail reset image
  //   rdy for swat
  if (!inSwipeModeRef.current) {
    setImgKey(CAT_BONK_KEY)
    inSwipeModeRef.current = setTimeout(
      () => {
        //   console.log('swipe mode over. jail for babies begins.')
        inSwipeModeRef.current = undefined
        inBabyJailRef.current = setTimeout(
          () => {
            setImgKey(CAT_REST_KEY)
            inBabyJailRef.current = undefined
          },
          Math.random() * 300 + 100,
        )
      },
      Math.random() * 100 + 100,
    )
  }

  // swat
  const adjustment = (Math.random() * Math.PI) / 8 - Math.PI / 16
  const adjustedAngle = rotate(adjustment, collision.normal1)
  const contactPoint = collision.point2
  const swipeForce = vectorScale(
    adjustedAngle,
    (catMass * 8) / Math.max(ball.invMass(), Number.MIN_VALUE),
  )
  ball.applyImpulseAtPoint(swipeForce, contactPoint, true)
}

export function CatSwat({
  id,
  onSelect,
  x,
  y,
  angle,
  catMass,
}: CatSwatWidget & EditableWidget) {
  const [imgKey, setImgKey] = useState(CAT_REST_KEY)

  const bodyRef = useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType, ActiveEvents }) => {
      return {
        key: id,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Fixed)
          .setTranslation(...coords.toRapier.vector(x, y))
          .setAdditionalMassProperties(
            catMass,
            pointToVectorObject(basis, 106, 116),
            catMass,
          )
          .setCcdEnabled(true)
          .setRotation(coords.toRapier.angle(angle)),
        colliderDescs: [
          // the cats silly little tail and body
          ...CAT_PATH.map((path) =>
            lineCuboid(ColliderDesc, path, basis).setDensity(0),
          ),
          // the cats head - can trigger swat
          Ball(ColliderDesc.ball.bind(undefined), basis, 32, {
            x: 129,
            y: 54,
          }).setActiveEvents(
            ActiveEvents.COLLISION_EVENTS | ActiveEvents.CONTACT_FORCE_EVENTS,
          ),
          // the cats bbutt - can trigger swat
          Ball(ColliderDesc.ball.bind(undefined), basis, 45, {
            x: 107,
            y: 136,
          }).setActiveEvents(
            ActiveEvents.COLLISION_EVENTS | ActiveEvents.CONTACT_FORCE_EVENTS,
          ),
          // swat sensor - make sure this is last
          Ball(ColliderDesc.ball.bind(undefined), basis, 126, {
            x: 126,
            y: 109,
          })
            .setSensor(true)
            .setActiveEvents(
              ActiveEvents.COLLISION_EVENTS | ActiveEvents.CONTACT_FORCE_EVENTS,
            ),
        ],
      }
    },
    [angle, x, y, catMass, id],
  )

  // grab the collider(s) we need off the body when they are ready for use
  const [swatCollider, setSwatCollider] = useState<Collider | undefined>()
  const [swatCollider2, setSwatCollider2] = useState<Collider | undefined>()
  const [swatCollider3, setSwatCollider3] = useState<Collider | undefined>()
  useRapierEffect(() => {
    const numColliders = bodyRef.current?.numColliders() || 0
    if (numColliders > 0) {
      const swat1 = bodyRef.current?.collider(numColliders - 1)
      const swat2 = bodyRef.current?.collider(numColliders - 2)
      const swat3 = bodyRef.current?.collider(numColliders - 3)
      setSwatCollider(swat1)
      setSwatCollider2(swat2)
      setSwatCollider3(swat3)
    }
  }, [bodyRef, setSwatCollider, x, y, angle])

  const inBabyJailRef = useRef<NodeJS.Timeout | undefined>()
  const inSwipeModeRef = useRef<NodeJS.Timeout | undefined>(undefined)
  useEffect(() => {
    return () => {
      clearTimeout(inBabyJailRef.current)
      clearTimeout(inSwipeModeRef.current)
    }
  }, [])

  // way to many refs
  // and function arguments
  useCollisionHandler(
    'start',
    swatCollider,
    (ballCollider) => {
      performSwat(
        swatCollider,
        ballCollider,
        inBabyJailRef,
        inSwipeModeRef,
        setImgKey,
        catMass,
      )
    },
    [setImgKey, inSwipeModeRef, inBabyJailRef, swatCollider, catMass, bodyRef],
  )
  useCollisionHandler(
    'start',
    swatCollider2,
    (ballCollider) => {
      performSwat(
        swatCollider2,
        ballCollider,
        inBabyJailRef,
        inSwipeModeRef,
        setImgKey,
        catMass,
      )
    },
    [setImgKey, inSwipeModeRef, inBabyJailRef, swatCollider2, catMass, bodyRef],
  )

  useCollisionHandler(
    'start',
    swatCollider3,
    (ballCollider) => {
      performSwat(
        swatCollider3,
        ballCollider,
        inBabyJailRef,
        inSwipeModeRef,
        setImgKey,
        catMass,
      )
    },
    [setImgKey, inSwipeModeRef, inBabyJailRef, swatCollider3, catMass, bodyRef],
  )

  return (
    <ComicImageAnimation
      {...useSelectHandlers(id, onSelect)}
      imgs={imgs}
      showIdx={imgKey}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
