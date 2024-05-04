import imgAttractor from '@art/attractor_4x.png'
import imgRepulsor from '@art/repulsor_4x.png'
import { useState } from 'react'
import { coords, vectorAngle, vectorDistance } from '../../lib/coords'
import { Sized, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { useSensorInTile } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider, useLoopHandler } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'

export interface AttractorWidget extends Vector, Sized {
  type: 'attractor'
}

export interface RepulsorWidget extends Vector, Sized {
  type: 'repulsor'
}

export function AttractorPreview() {
  return <ComicImage img={imgAttractor} />
}

export function RepulsorPreview() {
  return <ComicImage img={imgRepulsor} />
}

const startTime = performance.now()

export function AttractorRepulsor({
  id,
  onSelect,
  isSelected,
  className,
  x,
  y,
  width,
  strength,
}: Vector & Sized & EditableWidget & { className?: string; strength: number }) {
  const fieldSize = width
  const radius = 10

  const img = strength < 0 ? imgAttractor : imgRepulsor

  const boxCollider = useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.ball(coords.toRapier.length(radius))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRestitution(0.5),
    [radius, x, y],
  )

  const fieldCollider = useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.ball(coords.toRapier.length(fieldSize))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setSensor(true),
    [fieldSize, x, y],
  )

  const falloffDistance = coords.toRapier.length(fieldSize)
  const [angle, setAngle] = useState(0)
  const [scale, setScale] = useState(1)

  useSensorInTile(
    fieldCollider,
    (otherCollider) => {
      const body = otherCollider.parent()
      if (!boxCollider || !boxCollider.isValid() || !body) {
        return
      }

      const distance = vectorDistance(
        boxCollider.translation(),
        body.translation(),
      )

      const angle = vectorAngle(boxCollider.translation(), body.translation())

      const falloff = Math.max(
        0,
        Math.pow((falloffDistance - distance) / falloffDistance, 2),
      )

      const forceVector = {
        x: strength * falloff * Math.cos(angle),
        y: strength * falloff * Math.sin(angle),
      }

      body.applyImpulse(forceVector, true)
    },
    [boxCollider, falloffDistance, strength],
  )

  useLoopHandler(() => {
    const currentTime = performance.now()

    if (strength < 0) {
      setAngle(((360 * (currentTime - startTime)) / 4000) % 360)
      setScale(1 + 0.08 * Math.sin((Math.PI * (currentTime - startTime)) / 900))
    } else {
      setAngle(360 - (((360 * (currentTime - startTime)) / 20000) % 360))
      setScale(1 + 0.05 * Math.sin((Math.PI * (currentTime - startTime)) / 500))
    }
  }, [strength])

  return (
    <div
      {...useSelectHandlers(id, onSelect)}
      css={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        background: isSelected
          ? 'radial-gradient(circle closest-side at center, transparent, rgba(0, 0, 0, .25))'
          : 'transparent',
        width: fieldSize,
        height: fieldSize,
        borderRadius: '100%',
      }}
      style={getPositionStyles(x, y, 0)}
    >
      <ComicImage
        img={img}
        style={{
          transform: `rotate(-${angle}deg) scale(${scale})`,
        }}
      />
      <div
        css={{
          position: 'absolute',
          width: radius,
          height: radius,
        }}
        className={className}
        style={{
          borderRadius: '100%',
        }}
      />
    </div>
  )
}

export function Attractor(props: AttractorWidget & EditableWidget) {
  return (
    <AttractorRepulsor css={{}} {...props} strength={-0.1}></AttractorRepulsor>
  )
}

export function Repulsor(props: RepulsorWidget & EditableWidget) {
  return <AttractorRepulsor css={{}} {...props} strength={0.5} />
}
