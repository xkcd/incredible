import imgConstruction1 from '@art/construction-1_4x.png'
import imgConstruction2 from '@art/construction-2_4x.png'
import imgConstruction3 from '@art/construction-3_4x.png'
import imgConstruction4 from '@art/construction-4_4x.png'
import imgConstruction5 from '@art/construction-5_4x.png'
import imgConstruction6 from '@art/construction-6_4x.png'
import imgConstruction7 from '@art/construction-7_4x.png'
import imgConstruction8 from '@art/construction-8_4x.png'
import imgConstruction9 from '@art/construction-9_4x.png'
import { random, sampleSize } from 'lodash'
import { useMemo } from 'react'
import { coords } from '../lib/coords'
import { isBall } from '../types'
import { ComicImage } from './ComicImage'
import { useMachine } from './MachineContext'
import { useCollider, useCollisionHandler } from './PhysicsContext'
import { BALL_RADIUS } from './constants'

const imgConstructionChoices = [
  imgConstruction1,
  imgConstruction2,
  imgConstruction3,
  imgConstruction4,
  imgConstruction5,
  imgConstruction6,
  imgConstruction7,
  imgConstruction8,
  imgConstruction9,
]

export default function MachineTilePlaceholder({
  tileWidth,
  tileHeight,
  xt,
  yt,
}: {
  tileWidth: number
  tileHeight: number
  xt: number
  yt: number
}) {
  const { destroyBall } = useMachine()

  // If balls exit finished tiles into the placeholder, destroy them.
  const ballCollider = useCollider(
    ({ ColliderDesc, ActiveEvents }) =>
      ColliderDesc.cuboid(
        ...coords.toRapier.lengths(
          tileWidth / 2 - BALL_RADIUS * 2,
          tileHeight / 2 - BALL_RADIUS * 2,
        ),
      )
        .setTranslation(
          ...coords.toRapier.vector(
            xt * tileWidth + tileWidth / 2,
            yt * tileHeight + tileHeight / 2,
          ),
        )
        .setSensor(true)
        .setActiveEvents(ActiveEvents.COLLISION_EVENTS),
    [tileHeight, tileWidth, xt, yt],
  )

  useCollisionHandler(
    'start',
    ballCollider,
    (otherCollider) => {
      const body = otherCollider.parent()
      if (!body) {
        return
      }

      if (isBall(body)) {
        destroyBall(body.userData.id)
      }
    },
    [],
  )

  const imgs = useMemo(
    () => sampleSize(imgConstructionChoices, random(2, 3)),
    [],
  )

  return (
    <div
      css={{
        position: 'absolute',
        background: 'white',
        left: xt * tileWidth,
        top: yt * tileHeight,
        width: tileWidth,
        height: tileHeight,
        border: '1px solid black',
        boxSizing: 'border-box',
        pointerEvents: 'none',
        zIndex: 20,
      }}
    >
      {imgs.map((img, idx) => (
        <ComicImage
          key={idx}
          img={img}
          css={{
            position: 'absolute',
            left: 0,
            top: 0,
          }}
        />
      ))}
    </div>
  )
}
