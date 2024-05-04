import {
  HTMLAttributes,
  ImgHTMLAttributes,
  forwardRef,
  useCallback,
  useEffect,
  useRef,
} from 'react'
import { TICK_MS, useLoopHandler } from './PhysicsContext'

export interface ComicImageProps
  extends Omit<ImgHTMLAttributes<HTMLImageElement>, 'src' | 'srcSet'> {
  img: typeof import('*.png').default
  alt?: string
}

export const ComicImage = forwardRef<HTMLImageElement, ComicImageProps>(
  function ComicImage({ img, alt = '', ...props }, ref) {
    return (
      <img
        {...props}
        ref={ref}
        src={img.url['2x']}
        srcSet={img.srcSet}
        width={img.width}
        height={img.height}
        alt={alt}
        draggable="false"
      />
    )
  },
)

export const ComicImageAnimation = function ComicImageAnimation({
  imgs,
  rateMs = 0,
  showIdx = null,
  ...props
}: {
  imgs: Array<typeof import('*.png').default>
  rateMs?: number
  showIdx?: number | null
  animate?: boolean
} & HTMLAttributes<HTMLDivElement>) {
  const ref = useRef<HTMLImageElement>(null)
  const lastIdx = useRef<number | null>(null)

  const updateIdx = useCallback((idx: number) => {
    if (idx === lastIdx.current) {
      return
    }

    const { current: el } = ref
    if (!el) {
      return
    }
    if (lastIdx.current != null) {
      el.children[lastIdx.current].setAttribute('style', 'opacity: 0')
    }
    el.children[idx].setAttribute('style', 'opacity: 1')
    lastIdx.current = idx
  }, [])

  useEffect(() => {
    if (showIdx != null) {
      updateIdx(showIdx)
      return
    } else if (lastIdx.current == null) {
      // On first render, always show first frame, even if paused.
      updateIdx(0)
    }
  }, [imgs, rateMs, updateIdx, showIdx])

  const ticksPerFrame = rateMs / TICK_MS
  const isStatic = showIdx != null || rateMs === 0
  useLoopHandler(
    ({ getCurrentTick }) => {
      if (isStatic) {
        return
      }
      const currentTick = getCurrentTick()
      updateIdx(Math.floor(currentTick / ticksPerFrame) % imgs.length)
    },
    [imgs.length, isStatic, ticksPerFrame, updateIdx],
    !isStatic,
  )

  return (
    <div
      ref={ref}
      css={{
        position: 'relative',
        width: imgs[0].width,
        height: imgs[0].height,
        img: {
          position: 'absolute',
          left: 0,
          top: 0,
          opacity: 0,
          isolation: 'isolate',
        },
        contain: 'strict',
      }}
      {...props}
    >
      {imgs.map((img, idx) => (
        <ComicImage key={idx} img={img} />
      ))}
    </div>
  )
}
