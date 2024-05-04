import { coords } from '../../lib/coords'
import { RandallPath } from '../../lib/utils'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useRigidBody } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useLoopHandler, useRapierEffect } from '../PhysicsContext'
import { usePositionedBodyRef } from '../positionStyles'
import { lineCuboid } from './lib/lineCuboid'

import imgHook from '@art/hook_4x.png'

export interface HookWidgetBase extends Vector, Angled {
  type: string
  mass?: number
  restitution?: number
  damping?: number
  disableSpring?: boolean
  isLeft?: boolean
}

export interface HookWidget extends HookWidgetBase {
  type: 'hook'
}

export interface LeftHookWidget extends HookWidgetBase {
  type: 'lefthook'
}

export function HookPreview() {
  return (
    <ComicImage
      img={imgHook}
      css={{ width: '100%', height: 'auto', transform: 'rotate(-45deg)' }}
    />
  )
}

export function FlippedHookPreview() {
  return (
    <ComicImage
      img={imgHook}
      css={{
        width: '100%',
        height: 'auto',
        transform: 'scaleX(-1) rotate(45deg)',
      }}
    />
  )
}

const paths: RandallPath[] = [
  // * handle: 1,9 -> 206,9, width: 12
  { x1: 1, y1: 9, x2: 190, y2: 9, thickness: 9 },
  // * thickboy: 188,9 -> 206,9, width: 12
  { x1: 188, y1: 9, x2: 206, y2: 9, thickness: 12 },
  // * hookbuddyPt1: 202,16 -> 222,26, width: 6
  { x1: 202, y1: 16, x2: 222, y2: 26, thickness: 6 },
  // * hookbuddyPt2: 220,26 -> 237,14, width: 6
  { x1: 220, y1: 26, x2: 237, y2: 14, thickness: 6 },
  // * hookbuddyPt3: 237,2 -> 237,16, width: 6d
  { x1: 237, y1: 16, x2: 237, y2: 2, thickness: 6 },
]
const xBasis = imgHook.width / 2
const yBasis = imgHook.height / 2

function horizontalMirrorPaths(pathNodes: RandallPath[]): RandallPath[] {
  return pathNodes.map((value) => {
    return {
      x1: imgHook.width - value.x1,
      y1: value.y1,
      x2: imgHook.width - value.x2,
      y2: value.y2,
      thickness: value.thickness,
    }
  })
}

const mirrorPaths: RandallPath[] = horizontalMirrorPaths(paths)

export default function Hook({
  id,
  onSelect,
  x,
  y,
  mass = 1.3,
  restitution = 0.1,
  damping = 0.999,
  disableSpring = false,
  isLeft,
}: HookWidgetBase & EditableWidget) {
  const bodyRef = useRigidBody(
    ({ RigidBodyDesc, ColliderDesc, RigidBodyType }) => {
      return {
        key: id,
        bodyDesc: new RigidBodyDesc(RigidBodyType.Dynamic)
          .lockTranslations()
          .setAdditionalMassProperties(
            // initial mass
            mass,
            // initial center of mass

            isLeft
              ? coords.toRapier.vectorObject(-170.5 + xBasis, 9.5 - yBasis)
              : coords.toRapier.vectorObject(170.5 - xBasis, 9.5 - yBasis),
            // initial angular inertia
            mass,
          )
          .setAngularDamping(damping)
          .setCcdEnabled(true),
        colliderDescs: [
          ...(isLeft ? mirrorPaths : paths).map((path) =>
            lineCuboid(ColliderDesc, path, { xBasis, yBasis })
              .setRestitution(restitution)
              .setDensity(0),
          ),
        ],
      }
    },
    [id, mass, isLeft, damping, restitution],
  )

  useLoopHandler(
    (_) => {
      const { current: body } = bodyRef
      if (body == null) {
        return
      }

      if (disableSpring) {
        body.resetTorques(false)
        return
      }

      const rotation = body.rotation()
      if (Math.abs(rotation) > 0.001) {
        const springConstant = -mass * damping
        body.resetTorques(true)
        body.applyTorqueImpulse(rotation * springConstant, false)
      }
    },
    [bodyRef, damping, disableSpring, mass],
  )

  // Update position without recreating so angular momentum is preserved during edits
  useRapierEffect(() => {
    const { current: body } = bodyRef
    if (!body) {
      return
    }
    body.setTranslation(coords.toRapier.vectorObject(x, y), true)
  }, [bodyRef, x, y])

  return (
    <div
      ref={usePositionedBodyRef(bodyRef, {
        width: imgHook.width,
        height: imgHook.height,
        initialX: x,
        initialY: y,
      })}
      {...useSelectHandlers(id, onSelect)}
    >
      <ComicImage
        img={imgHook}
        style={{
          transform: isLeft ? 'scaleX(-1)' : undefined,
        }}
      />
    </div>
  )
}

export function LeftHook(props: LeftHookWidget & EditableWidget) {
  return <Hook {...props} isLeft />
}
