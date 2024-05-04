/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-argument */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable @typescript-eslint/no-unsafe-call */
import { coords, vectorAngle, vectorDistance, vectorMagnitude } from '../../lib/coords'
import { Sized, Vector } from '../../types'
import { useSensorInTile } from '../MachineTileContext'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'


export interface QuantumGateSlowWidget extends Vector, Sized {
  type: 'quantumgateslow'
}

export interface QuantumGateFastWidget extends Vector, Sized {
  type: 'quantumgatefast'
}


const throb1 = `
@keyframes throb1 {

  0% {

    background: radial-gradient(circle at center, purple 0, orange 8px)

  }
  
  6% {
    background: radial-gradient(circle at center, purple 0, orange 9px)
  }
  
  12% {
    background: radial-gradient(circle at center, purple 0, orange 10px)
  }

  18% {
    background: radial-gradient(circle at center, purple 0, orange 11px)
  }

  25% {
    background: radial-gradient(circle at center, purple 0, orange 12px)
  }

  34% {
    background: radial-gradient(circle at center, purple 0, orange 13px)
  }

  50% {
    background: radial-gradient(circle at center, purple 0, orange 14px)
  }

  
  64% {
    background: radial-gradient(circle at center, purple 0, orange 13px)
  }

  75% {
    background: radial-gradient(circle at center, purple 0, orange 12px)
  }

  81% {
    background: radial-gradient(circle at center, purple 0, orange 11px)
  }

  87% {
    background: radial-gradient(circle at center, purple 0, orange 10px)
  }

  93% {
    background: radial-gradient(circle at center, purple 0, orange 9px)
  }

  100% {

    background: radial-gradient(circle at center, purple 0, orange 8px)

  }

}
`

const throb2 = `
@keyframes throb2 {

  0% {

    background: radial-gradient(circle at center, cyan 0, red 8px)

  }
  
  6% {
    background: radial-gradient(circle at center, cyan 0, red 9px)
  }
  
  12% {
    background: radial-gradient(circle at center, cyan 0, red 10px)
  }

  18% {
    background: radial-gradient(circle at center, cyan 0, red 11px)
  }

  25% {
    background: radial-gradient(circle at center, cyan 0, red 12px)
  }

  34% {
    background: radial-gradient(circle at center, cyan 0, red 13px)
  }

  50% {
    background: radial-gradient(circle at center, cyan 0, red 14px)
  }

  
  64% {
    background: radial-gradient(circle at center, cyan 0, red 13px)
  }

  75% {
    background: radial-gradient(circle at center, cyan 0, red 12px)
  }

  81% {
    background: radial-gradient(circle at center, cyan 0, red 11px)
  }

  87% {
    background: radial-gradient(circle at center, cyan 0, red 10px)
  }

  93% {
    background: radial-gradient(circle at center, cyan 0, red 9px)
  }

  100% {

    background: radial-gradient(circle at center, cyan 0, red 8px)

  }

}
`

export function QuantumGateSlowPreview() {


  return (
    <div>
    <style>{throb1}</style>
    <div
    style={{
      width: '22px',
      height: '22px',
      borderRadius: '24px',
      animation: 'throb1 1.5s linear infinite'
    }}
    ></div>
    </div>
  )
}

export function QuantumGateFastPreview() {
  return (
    <div>
    <style>{throb2}</style>
    <div
    style={{
      width: '18px',
      height: '18px',
      borderRadius: '24px',
      animation: 'throb2 1s linear infinite'
    }}
    ></div>
    </div>
  )
}


export function QuantumGate({
  id,
  onSelect,
  isSelected,
  className,
  x,
  y,
  width,
  strength,
  speedLimit,
}: Vector & Sized & EditableWidget & { className?: string; strength: number; speedLimit: number }) {

  const fieldSize = width
  const radius = 10

  const boxCollider = useCollider(
    ({ ColliderDesc }) =>
      ColliderDesc.ball(coords.toRapier.length(radius))
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRestitution(0.5)
        .setSensor(true),
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

  useSensorInTile(
    fieldCollider,
    (otherCollider) => {
      
      const body = otherCollider.parent()
      if (!boxCollider || !body) {
        return
      }

      const speedLimitRapier = coords.toRapier.length( speedLimit )
      const speed = vectorMagnitude( body.linvel() ) * Math.sign( speedLimit )

      if ( speed < speedLimitRapier ) {

        const distance = vectorDistance(
          boxCollider.translation(),
          body.translation(),
        )

        const angle = vectorAngle(boxCollider.translation(), body.translation())

        const falloff = Math.max(
          0,
          Math.pow((falloffDistance - distance) / falloffDistance, 3),
        )

        const forceVector = {
          x: strength * falloff * Math.cos(angle),
          y: strength * falloff * Math.sin(angle),
        }

        body.applyImpulse(forceVector, true)
      }
    },
    [boxCollider, falloffDistance, strength, speedLimit],
  )

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
      style={getPositionStyles(x, y)}
    >
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

export function QuantumGateSlow(props: QuantumGateSlowWidget & EditableWidget) {
  return (
    <div>
    <style>{throb1}</style>
    <QuantumGate
      css={{
        animation: 'throb1 1.5s linear infinite',
        width: '22px',
        height: '22px',
        borderRadius: '24px',
      }}
      {...props}
      strength={10}
      speedLimit={-400}
    />
    </div>
  )
}

export function QuantumGateFast(props: QuantumGateFastWidget & EditableWidget) {
  return (
    <div>
    <style>{throb2}</style> 
    <QuantumGate
      css={{
        animation: 'throb2 1s linear infinite',
        width: '18px',
        height: '18px',
        borderRadius: '24px',
      }}
      {...props}
      strength={3}
      speedLimit={200}
    />
    </div>
  )
}
