import { useContext, useRef } from 'react'
import { coords } from '../lib/coords'
import { MachineTileContext } from './MachineTileContext'
import { useLoopHandler } from './PhysicsContext'

export default function DebugOverlay({
  ticksBetweenUpdates = 0,
}: {
  ticksBetweenUpdates?: number
}) {
  const canvasRef = useRef<HTMLCanvasElement | null>(null)
  const { width, height } = useContext(MachineTileContext)

  const ticksRef = useRef<number | null>(null)

  useLoopHandler(
    ({ world }) => {
      const context = canvasRef.current?.getContext('2d')
      if (!context) {
        return
      }

      const { current: ticksSinceUpdate } = ticksRef
      if (
        ticksBetweenUpdates > 0 &&
        ticksSinceUpdate != null &&
        ticksSinceUpdate < ticksBetweenUpdates
      ) {
        ticksRef.current = ticksSinceUpdate + 1
        return
      }

      context.clearRect(0, 0, width, height)
      const { vertices, colors } = world.debugRender()

      for (let i = 0; i < vertices.length / 4; i += 1) {
        const red = colors[i * 4 + 0] * 255
        const green = colors[i * 4 + 1] * 255
        const blue = colors[i * 4 + 2] * 255
        const alpha = colors[i * 4 + 3] * 255

        context.beginPath()
        context.strokeStyle = `rgba(${red}, ${green}, ${blue}, ${alpha})`
        context.lineWidth = 1.5
        context.moveTo(
          ...coords.fromRapier.vector(vertices[i * 4 + 0], vertices[i * 4 + 1]),
        )
        context.lineTo(
          ...coords.fromRapier.vector(vertices[i * 4 + 2], vertices[i * 4 + 3]),
        )
        context.closePath()
        context.stroke()
      }

      ticksRef.current = 0
    },
    [width, height, ticksRef, ticksBetweenUpdates],
  )

  return (
    <div
      css={{
        position: 'relative',
        width: '100%',
        height: '100%',
        pointerEvents: 'none',
        zIndex: 100,
      }}
    >
      <canvas ref={canvasRef} height={width} width={height} />
    </div>
  )
}
