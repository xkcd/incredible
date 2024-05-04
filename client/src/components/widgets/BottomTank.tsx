import tankBlueOpenImg from '@art/tank-blue-open_4x.png'
import tankBlueImg from '@art/tank-blue_4x.png'
import tankGreenOpenImg from '@art/tank-green-open_4x.png'
import tankGreenImg from '@art/tank-green_4x.png'
import tankRedOpenImg from '@art/tank-red-open_4x.png'
import tankRedImg from '@art/tank-red_4x.png'
import tankYellowOpenImg from '@art/tank-yellow-open_4x.png'
import tankYellowImg from '@art/tank-yellow_4x.png'
import { useMemo, useState } from 'react'
import { coords } from '../../lib/coords'
import { BallType, Vector, isBall } from '../../types'
import { ComicImageAnimation } from '../ComicImage'
import { useMachine } from '../MachineContext'
import {
  TICK_MS,
  useCollider,
  useCollisionHandler,
  useLoopHandler,
} from '../PhysicsContext'
import { BOTTOM_TANK_HEIGHT, BOTTOM_TANK_WIDTH } from '../constants'
import { getPositionStyles } from '../positionStyles'
import { lineCuboid } from './lib/lineCuboid'

const tankImages = [
  [tankBlueImg, tankBlueOpenImg],
  [tankRedImg, tankRedOpenImg],
  [tankGreenImg, tankGreenOpenImg],
  [tankYellowImg, tankYellowOpenImg],
]

const intervalTicks = Math.floor((12 * 1000) / TICK_MS)
const openTime = Math.floor(800 / TICK_MS)

export function BottomTank({
  x,
  y,
  type,
}: {
  type: BallType
} & Vector) {
  const { killBall } = useMachine()
  const [isOpen, setOpen] = useState(false)

  const basis = useMemo(() => ({ xBasis: -x, yBasis: -y }), [x, y])

  // Left top funnel
  useCollider(
    ({ ColliderDesc, CoefficientCombineRule }) =>
      lineCuboid(
        ColliderDesc,
        { x1: -43, y1: -209, x2: -28, y2: -180, thickness: 2 },
        basis,
      )
        .setRestitution(0.05)
        .setRestitutionCombineRule(CoefficientCombineRule.Min),
    [basis],
  )

  // Right top funnel
  useCollider(
    ({ ColliderDesc, CoefficientCombineRule }) =>
      lineCuboid(
        ColliderDesc,
        { x1: 43, y1: -209, x2: 28, y2: -180, thickness: 2 },
        basis,
      )
        .setRestitution(0.05)
        .setRestitutionCombineRule(CoefficientCombineRule.Min),
    [basis],
  )

  // Left top
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: -85, y1: -164, x2: -32, y2: -190, thickness: 17 },
        basis,
      ),
    [basis],
  )

  // Right top
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: 85, y1: -164, x2: 32, y2: -190, thickness: 17 },
        basis,
      ),
    [basis],
  )

  // Left edge
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: -79, y1: -170, x2: -79, y2: 205, thickness: 20 },
        basis,
      ),
    [basis],
  )

  // Right edge
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: 79, y1: -170, x2: 79, y2: 205, thickness: 20 },
        basis,
      ),
    [basis],
  )

  // Left lip
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: -86, y1: 172, x2: -86, y2: 205, thickness: 20 },
        basis,
      ),
    [basis],
  )

  // Right lip
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: 82, y1: 164, x2: 82, y2: 209, thickness: 26 },
        basis,
      ),
    [basis],
  )

  // Bottom bar
  useCollider(
    ({ ColliderDesc }) =>
      lineCuboid(
        ColliderDesc,
        { x1: -82, y1: 188, x2: isOpen ? -82 : 82, y2: 188, thickness: 7 },
        basis,
      ),
    [basis, isOpen],
  )

  const sensorCollider = useCollider(
    ({ ColliderDesc, ActiveEvents }) =>
      ColliderDesc.cuboid(
        ...coords.toRapier.lengths(
          BOTTOM_TANK_WIDTH / 2 - 26,
          BOTTOM_TANK_HEIGHT / 2 - 26,
        ),
      )
        .setTranslation(...coords.toRapier.vector(x, y + 10))
        .setSensor(true)
        .setActiveEvents(ActiveEvents.COLLISION_EVENTS),
    [x, y],
  )

  useCollisionHandler(
    'start',
    sensorCollider,
    (otherCollider) => {
      const body = otherCollider.parent()
      if (!body || !isBall(body)) {
        return
      }

      if (body.userData.ballType !== type) {
        killBall(body.userData.id)
      }
    },
    [],
  )

  useLoopHandler(({ getCurrentTick }) => {
    const currentTick = getCurrentTick()
    if (currentTick % intervalTicks === 0) {
      setOpen(true)
    }
    if (currentTick % intervalTicks === openTime) {
      setOpen(false)
    }
  }, [])

  return (
    <ComicImageAnimation
      imgs={tankImages[type - 1]}
      showIdx={isOpen ? 1 : 0}
      style={getPositionStyles(x, y)}
    />
  )
}
