import { animate, motion, useMotionValue, useTransform } from 'framer-motion'
import {
  ComponentProps,
  forwardRef,
  useCallback,
  useEffect,
  useImperativeHandle,
  useMemo,
  useState,
} from 'react'
import { Bounds } from '../types'

export interface SlippyMapRef {
  getPosition(): [x: number, y: number]
  isAnimating(): boolean
  animateTo(x: number, y: number, zoom?: number): Promise<void>
  jumpTo(x: number, y: number, zoom?: number): void
  stop(): void
}

export interface CenteredSlippyMapProps {
  width: number
  height: number
  totalWidth: number
  totalHeight: number
  innerClassName?: string
  onPosition: (viewBounds: Bounds) => void
  onDragStart?: ComponentProps<typeof motion.div>['onDragStart']
  children: React.ReactElement
  initialX?: number
  initialY?: number
  initialZoom?: number
}

export const CenteredSlippyMap = forwardRef<
  SlippyMapRef,
  CenteredSlippyMapProps
>(function CenteredSlippyMap(
  {
    width,
    height,
    totalWidth,
    totalHeight,
    innerClassName,
    onPosition,
    onDragStart,
    children,
    initialX = 0,
    initialY = 0,
    initialZoom = 1,
  }: CenteredSlippyMapProps,
  ref,
) {
  const scaleVal = useMotionValue(initialZoom)
  const [scale, setScale] = useState(initialZoom)

  // The input position in unzoomed child screen space, centered in viewport.
  // We need to scale it by the zoom level into screen space.
  const centerXVal = useMotionValue(-initialX * initialZoom)
  const centerYVal = useMotionValue(-initialY * initialZoom)

  // Translation offset in screen space of drag gesture
  const dragXVal = useTransform(() => centerXVal.get() + 0.5 * width)
  const dragYVal = useTransform(() => centerYVal.get() + 0.5 * height)

  const handleUpdate = useCallback(() => {
    const x = dragXVal.get()
    const y = dragYVal.get()
    const scale = scaleVal.get()

    const scaleFactor = 1 / scale
    const viewBounds: Bounds = [
      -x * scaleFactor,
      -y * scaleFactor,
      (-x + width) * scaleFactor,
      (-y + height) * scaleFactor,
    ]
    onPosition(viewBounds)
  }, [dragXVal, dragYVal, scaleVal, width, height, onPosition])

  // Initial set of bounds on mount
  useEffect(handleUpdate, [handleUpdate])

  const getPosition = useCallback(() => {
    return [
      -centerXVal.get() / scaleVal.get(),
      -centerYVal.get() / scaleVal.get(),
    ] as [number, number]
  }, [centerXVal, centerYVal, scaleVal])

  const isAnimating = useCallback(() => {
    return centerXVal.isAnimating() || centerYVal.isAnimating()
  }, [centerXVal, centerYVal])

  const animateTo = useCallback(
    async (x: number, y: number, zoom: number = 1) => {
      await Promise.all([
        setScale(zoom),
        animate(scaleVal, zoom),
        animate(centerXVal, -x * zoom),
        animate(centerYVal, -y * zoom),
      ])
    },
    [scaleVal, centerXVal, centerYVal],
  )

  const jumpTo = useCallback(
    (x: number, y: number, zoom: number = 1) => {
      setScale(zoom)
      scaleVal.set(zoom)
      centerXVal.set(-x * zoom)
      centerYVal.set(-y * zoom)
    },
    [scaleVal, centerXVal, centerYVal],
  )

  const stop = useCallback(() => {
    scaleVal.stop()
    centerXVal.stop()
    centerYVal.stop()
  }, [scaleVal, centerXVal, centerYVal])

  useImperativeHandle(ref, () => ({
    getPosition,
    isAnimating,
    animateTo,
    jumpTo,
    stop,
  }))

  const dragConstraints = useMemo(() => {
    const right = -0.5 * scale * width
    const left = right - (totalWidth - width) * scale
    const bottom = -0.5 * scale * height
    const top = bottom - (totalHeight - height) * scale
    return {
      left,
      right,
      top,
      bottom,
    }
  }, [height, scale, totalHeight, totalWidth, width])

  return (
    <div
      css={{
        overflow: 'hidden',
        width,
        height,
        userSelect: 'none',
        // Allow framer to handle touch pans
        touchAction: 'none',
      }}
    >
      <motion.div
        css={{
          position: 'relative',
          width: totalWidth,
          height: totalHeight,
          overflow: 'hidden',
          contain: 'strict',
        }}
        style={{
          x: dragXVal,
          y: dragYVal,
          scale: scaleVal,
          originX: 0,
          originY: 0,
        }}
        className={innerClassName}
        whileDrag={{ cursor: 'grabbing' }}
        onDragStart={onDragStart}
        onUpdate={handleUpdate}
        dragConstraints={dragConstraints}
        _dragX={centerXVal}
        _dragY={centerYVal}
        drag
      >
        {children}
      </motion.div>
    </div>
  )
})
