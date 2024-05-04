import imgBottomChute from '@art/chute-0v_4x.png'
import imgBottomChute25L from '@art/chute-25l_4x.png'
import imgBottomChute25R from '@art/chute-25r_4x.png'
import imgBottomChute50L from '@art/chute-50l_4x.png'
import imgBottomChute50R from '@art/chute-50r_4x.png'
import imgBottomChute75L from '@art/chute-75l_4x.png'
import imgBottomChute75R from '@art/chute-75r_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Ball, Vector, isBall } from '../../types'
import { ComicImage } from '../ComicImage'
import { useMachine } from '../MachineContext'
import { useCollider, useCollisionHandler } from '../PhysicsContext'
import { BOTTOM_CHUTE_EXIT_OFFSET } from '../constants'
import { getPositionStyles } from '../positionStyles'

export function BottomChute({
  x,
  y,
  angle,
  onReceiveBall,
}: {
  onReceiveBall: (ball: Ball) => void
} & Angled &
  Vector) {
  const { destroyBall } = useMachine()

  const width = imgBottomChute.width
  const height = imgBottomChute.height

  const sensorCollider = useCollider(
    ({ ColliderDesc, ActiveEvents }) =>
      ColliderDesc.cuboid(...coords.toRapier.lengths(width / 2, height / 8))
        .setTranslation(
          ...coords.toRapier.vector(x, y + BOTTOM_CHUTE_EXIT_OFFSET),
        )
        .setSensor(true)
        .setActiveEvents(ActiveEvents.COLLISION_EVENTS),
    [height, width, x, y],
  )

  useCollisionHandler(
    'start',
    sensorCollider,
    (otherCollider) => {
      const body = otherCollider.parent()
      if (!body || !isBall(body)) {
        return
      }

      onReceiveBall(body)

      destroyBall(body.userData.id)
    },
    [],
  )

  const angleDeg = angle * (180 / Math.PI) - 90
  let img = imgBottomChute
  if (angleDeg < -40) {
    img = imgBottomChute75R
  } else if (angleDeg < -10) {
    img = imgBottomChute50R
  } else if (angleDeg < -2) {
    img = imgBottomChute25R
  } else if (angleDeg < 2) {
    img = imgBottomChute
  } else if (angleDeg < 10) {
    img = imgBottomChute25L
  } else if (angleDeg < 40) {
    img = imgBottomChute50L
  } else {
    img = imgBottomChute75L
  }

  return (
    <ComicImage
      img={img}
      css={{ zIndex: 20 }}
      style={getPositionStyles(x, y)}
    />
  )
}
