import useSize from '@react-hook/size'
import {
  ReactNode,
  createContext,
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react'
import comic from '../../comic.json'
import InnerComicBorder from './InnerComicBorder'
import { SwooshyDialog } from './SwooshyDialog'

function useIsFullscreen() {
  const [isFullscreen, setFullscreen] = useState(false)

  useEffect(() => {
    function handleFullscreenChange() {
      setFullscreen(
        Boolean(document.fullscreenElement ?? document.webkitFullscreenElement),
      )
    }

    document.addEventListener('fullscreenchange', handleFullscreenChange)
    return () => {
      document.removeEventListener('fullscreenchange', handleFullscreenChange)
    }
  })

  return isFullscreen
}

export type DisplayContextType = {
  isFullscreen: boolean
  orientation: 'portrait' | 'landscape'
}

export const DisplayContext = createContext<DisplayContextType>({
  isFullscreen: false,
  orientation: 'portrait',
})

export function useDisplayState() {
  return useContext(DisplayContext)
}

export function FullscreenComicContainer({
  children,
}: {
  children: ReactNode
}) {
  const ref = useRef<HTMLDivElement>(null)

  const [canFullscreen, setCanFullscreen] = useState(false)
  const [fullscreenFailed, setFullscreenFailed] = useState(false)

  const [width, height] = useSize(ref)

  const isFullscreen = useIsFullscreen()

  const [isTouch] = useState(
    () => window.matchMedia('(pointer: coarse)').matches,
  )

  useEffect(() => {
    const { current: el } = ref
    if (!el) {
      return
    }
    setCanFullscreen(
      'requestFullscreen' in el || 'webkitRequestFullscreen' in el,
    )
  }, [])

  const handleMaybeFullscreen = useCallback(() => {
    const { current: el } = ref

    async function tryFullscreen() {
      if (!el) {
        return
      }

      if (
        document.fullscreenElement !== el &&
        document.webkitFullscreenElement !== el
      ) {
        try {
          if ('requestFullscreen' in el) {
            await el.requestFullscreen?.()
          } else if ('webkitRequestFullscreen' in el) {
            await el.webkitRequestFullscreen?.()
          }
        } catch {
          setFullscreenFailed(true)
        }
      }
    }
    void tryFullscreen()
  }, [])

  const comicScale = Math.min(width / comic.width, height / comic.height)

  return (
    <div
      ref={ref}
      css={{
        position: 'relative',
        width: '100%',
        height: '100%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        background: 'white',
      }}
    >
      <InnerComicBorder
        style={
          comicScale !== 1
            ? {
                transform: `scale(${comicScale})`,
              }
            : undefined
        }
      >
        {isTouch && !isFullscreen && !fullscreenFailed && canFullscreen && (
          <SwooshyDialog
            css={{
              zIndex: 100,
            }}
            onDismiss={handleMaybeFullscreen}
          >
            <button
              onClick={handleMaybeFullscreen}
              css={{
                width: 365,
                padding: 16,
                fontFamily: 'xkcd-Regular-v3',
                fontSize: 50,
                background: 'rgba(0, 0, 0, .75)',
                color: 'white',
                border: 'none',
                borderRadius: 16,
              }}
            >
              tap to enter fullscreen
            </button>
          </SwooshyDialog>
        )}
        <DisplayContext.Provider
          value={{
            isFullscreen,
            orientation: height > width ? 'portrait' : 'landscape',
          }}
        >
          {children}
        </DisplayContext.Provider>
      </InnerComicBorder>
    </div>
  )
}
