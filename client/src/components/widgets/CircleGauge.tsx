import { clamp } from 'lodash'
import { SVGAttributes } from 'react'

const PI_2 = 2 * Math.PI

export function CircleGauge({
  value: rawValue,
  lineWidth = 0.2,
  ...props
}: { value: number; lineWidth?: number } & SVGAttributes<SVGElement>) {
  const value = clamp(rawValue, 0, 1)
  const largeArc = value > 0.5 ? 1 : 0
  const halfLineWidth = lineWidth / 2
  const radius = 1 - halfLineWidth
  const x = radius * Math.sin(value * PI_2) + 1
  const y = 1 - radius * Math.cos(value * PI_2)

  return (
    <svg {...props} viewBox="0 0 2 2">
      {value === 1 ? (
        <circle
          r={radius}
          cx={1}
          cy={1}
          strokeWidth={lineWidth}
          fill="transparent"
        />
      ) : (
        <path
          d={`M 1 ${halfLineWidth} A ${radius} ${radius} 0 ${largeArc} 1 ${x} ${y}`}
          strokeWidth={lineWidth}
          fill="transparent"
        />
      )}
    </svg>
  )
}
