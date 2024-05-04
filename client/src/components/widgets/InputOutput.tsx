import imgRoller1 from '@art/one-roller-1_4x.png'
import imgRoller2 from '@art/one-roller-2_4x.png'
import imgRoller3 from '@art/one-roller-3_4x.png'
import imgRoller4 from '@art/one-roller-4_4x.png'
import imgRoller5 from '@art/one-roller-5_4x.png'
import type { ColliderDesc } from '@dimforge/rapier2d'
import { sample } from 'lodash'
import { useMemo } from 'react'
import { coords } from '../../lib/coords'
import {
  BallData,
  BallTypeRate,
  PuzzlePosition,
  Vector,
  isBall,
} from '../../types'
import { ComicImage } from '../ComicImage'
import { useMachine } from '../MachineContext'
import { useRigidBody } from '../MachineTileContext'
import { useCollider, useCollisionHandler } from '../PhysicsContext'
import {
  INPUT_SPINNER_SIZE,
  INPUT_SPINNER_SPEED,
  INPUT_TEETH_COUNT,
  INPUT_WIDTH,
} from '../constants'
import { getPositionStyles, usePositionedBodyRef } from '../positionStyles'
import { ballClassNames } from './Balls'

const imgRollerChoices = [
  imgRoller1,
  imgRoller2,
  imgRoller3,
  imgRoller4,
  imgRoller5,
]

export type InputOutputSide = 'left' | 'top' | 'right' | 'bottom'

export function positionToSide(
  position: PuzzlePosition,
): InputOutputSide | undefined {
  if (position.y > 0 && position.y < 1) {
    if (position.x === 0) {
      return 'left'
    } else if (position.x === 1) {
      return 'right'
    }
  } else {
    if (position.y === 0) {
      return 'top'
    } else if (position.y === 1) {
      return 'bottom'
    }
  }
}

function Triangle({ className }: { className?: string }) {
  // Thanks to https://blog.kalehmann.de/blog/2020/09/10/css-centered-equilateral-triangle.html
  return (
    <svg viewBox="0 0 120 120" className={className}>
      <polygon points="60 95, 10 10, 110 10" />
    </svg>
  )
}

function TypeIndicators({
  x,
  y,
  balls,
  side,
}: {
  x: number
  y: number
  balls: BallTypeRate[]
  side: InputOutputSide
}) {
  const size = 12
  const offset = 36
  return (
    <div
      css={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        gap: 4,
        '&.top': {
          marginTop: offset,
        },
        '&.bottom': {
          marginTop: -offset,
        },
        '&.left, &.right': {
          flexDirection: 'column',
        },
        '&.left': {
          marginLeft: offset,
          svg: {
            transform: 'rotate(90deg)',
          },
        },
        '&.right': {
          marginLeft: -offset,
          svg: {
            transform: 'rotate(-90deg)',
          },
        },
      }}
      style={getPositionStyles(x, y)}
      className={side}
    >
      {balls.map(({ type }) => (
        <Triangle
          key={type}
          css={{
            height: size,
            stroke: 'black',
            strokeWidth: 10,
            opacity: 0.65,
            '&.blue': {
              fill: '#4057bf',
            },
            '&.red': {
              fill: '#ff1600',
            },
            '&.green': {
              fill: '#44bf40',
            },
            '&.yellow': {
              fill: '#f8eb06',
            },
          }}
          className={ballClassNames[type - 1] ?? undefined}
        />
      ))}
    </div>
  )
}

function Roller({
  x,
  y,
  rotationSpeed,
  spokes,
}: Vector & {
  rotationSpeed: number
  spokes?: number
}) {
  const innerCircleRadius = 14
  const img = useMemo(() => sample(imgRollerChoices)!, [])

  const bodyRef = useRigidBody(
    ({
      RigidBodyDesc,
      ColliderDesc,
      RigidBodyType,
      CoefficientCombineRule,
    }) => {
      const toothColliders: ColliderDesc[] =
        spokes != null
          ? Array(spokes)
              .fill(0)
              .map((_, index) => {
                return ColliderDesc.cuboid(
                  ...coords.toRapier.lengths(INPUT_SPINNER_SIZE / 2, 0.5),
                )
                  .setRotation((index * Math.PI) / spokes)
                  .setRestitution(0.01)
                  .setFriction(1)
              })
          : []
      return {
        key: null,
        bodyDesc: new RigidBodyDesc(RigidBodyType.KinematicVelocityBased)
          .setTranslation(...coords.toRapier.vector(x, y))
          .lockTranslations()
          .setAngvel(rotationSpeed),
        colliderDescs: [
          ColliderDesc.ball(coords.toRapier.length(innerCircleRadius))
            .setRestitution(0.01)
            .setRestitutionCombineRule(CoefficientCombineRule.Min),
          ...toothColliders,
        ],
      }
    },
    [spokes, x, y, rotationSpeed],
  )

  return (
    <ComicImage
      img={img}
      ref={usePositionedBodyRef(bodyRef, {
        width: INPUT_SPINNER_SIZE,
        height: INPUT_SPINNER_SIZE,
        initialX: x,
        initialY: y,
      })}
      css={{ zIndex: 10 }}
    />
  )
}

export default function InputOutput({
  x,
  y,
  side,
  balls,
  isInput,
  onReceiveBall,
}: {
  side: InputOutputSide
  isInput: boolean
  onReceiveBall?: (ballData: BallData) => void
} & PuzzlePosition) {
  const { renewBall } = useMachine()

  const isVertical = side === 'left' || side === 'right'

  const sensorCollider = useCollider(
    ({ ColliderDesc, ActiveEvents }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(INPUT_WIDTH / 4, 6))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRotation(isVertical ? Math.PI / 2 : 0)
        .setSensor(true)
        .setActiveEvents(ActiveEvents.COLLISION_EVENTS),
    [isVertical, x, y],
  )

  // Ramp to help feed vertical inputs
  useCollider(
    ({ ColliderDesc }) =>
      isInput && isVertical
        ? ColliderDesc.cuboid(...coords.toRapier.lengths(INPUT_WIDTH / 2, 2))
            .setTranslation(
              ...coords.toRapier.vector(
                side === 'left'
                  ? x - INPUT_SPINNER_SIZE * 1.5
                  : x + INPUT_SPINNER_SIZE * 1.5,
                y - INPUT_SPINNER_SIZE / 3,
              ),
            )
            .setRotation(side === 'left' ? -Math.PI / 6 : Math.PI / 6)
        : null,
    [isInput, isVertical, side, x, y],
  )

  useCollisionHandler(
    'start',
    sensorCollider,
    (otherCollider) => {
      const body = otherCollider.parent()
      if (!sensorCollider || !body) {
        return
      }

      if (!isBall(body)) {
        return
      }

      body.setLinvel({ x: 0, y: 0 }, true)
      body.setAngvel(0, true)

      renewBall(body.userData.id)
      onReceiveBall?.(body.userData)
    },
    [onReceiveBall, renewBall],
  )

  const offset = INPUT_WIDTH / 2 - INPUT_SPINNER_SIZE / 2
  const spinDirection =
    (isInput ? 1 : -1) * (side === 'left' || side === 'bottom' ? -1 : 1)

  return (
    <>
      {!isInput ? (
        <TypeIndicators x={x} y={y} balls={balls} side={side} />
      ) : null}
      <Roller
        x={isVertical ? x : x - offset}
        y={isVertical ? y - offset : y}
        rotationSpeed={-INPUT_SPINNER_SPEED * spinDirection}
        spokes={INPUT_TEETH_COUNT / 2}
      />
      <Roller
        x={isVertical ? x : x + offset}
        y={isVertical ? y + offset : y}
        rotationSpeed={INPUT_SPINNER_SPEED * spinDirection}
        spokes={INPUT_TEETH_COUNT / 2}
      />
    </>
  )
}
