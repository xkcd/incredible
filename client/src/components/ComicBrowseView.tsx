import imgFollowBall from '@art/follow-ball_4x.png'
import imgPermalink from '@art/permalink_4x.png'
import { AnimatePresence, motion } from 'framer-motion'
import { random } from 'lodash'
import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
} from 'react'
import { Bounds } from '../types'
import { MetaMachineLink } from './Comic'
import { ComicImage } from './ComicImage'
import LoadingSpinner from './LoadingSpinner'
import {
  SlippyMetaMachineRef,
  SlippyMetaMachineView,
  SlippyMetaMachineViewProps,
} from './MetaMachineView'
import { PhysicsContextProvider, PhysicsLoader } from './PhysicsContext'
import { ToolButton, comicDropShadow } from './WidgetPalette'
import { MetaMachineInfo, SavedMachine } from './useMetaMachineClient'

function findRandomTile(metaMachine: MetaMachineInfo) {
  const { tilesX, tilesY } = metaMachine

  for (let tries = 0; tries < 16; tries++) {
    const xt = random(0, tilesX - 1)
    const yt = random(0, tilesY - 1)
    if (metaMachine.hasBlueprint(xt, yt)) {
      return { xt, yt }
    }
  }

  // <3, jamslunt interfoggle
  return { xt: 2, yt: 2 }
}

export function ComicBrowseView({
  lastSubmission,
  metaMachine,
  isActive,
  onPosition,
  hashLink,
  updateHashLink,
}: {
  lastSubmission: SavedMachine | undefined
  metaMachine: MetaMachineInfo
  isActive: boolean
  onPosition: SlippyMetaMachineViewProps['onPosition']
  hashLink: MetaMachineLink | null
  updateHashLink: (link: MetaMachineLink) => void
}) {
  const viewRef = useRef<SlippyMetaMachineRef>(null)

  const getMap = () => viewRef?.current?.mapRef?.current
  const getMachine = () => viewRef?.current?.machineRef?.current

  const { tileWidth, tileHeight, tilesX, tilesY } = metaMachine

  function centeredScreenCoords({ xt, yt }: { xt: number; yt: number }) {
    return [xt * tileWidth + tileWidth / 2, yt * tileHeight + tileHeight / 2]
  }

  const [initialCenterX, initialCenterY] = centeredScreenCoords(
    lastSubmission ?? hashLink ?? findRandomTile(metaMachine),
  )

  const isMovingToSubmission = useRef(false)

  useLayoutEffect(() => {
    async function doSubmitAnimate() {
      if (lastSubmission) {
        isMovingToSubmission.current = true
        getMachine()?.unfollowBall()
        getMap()?.jumpTo(initialCenterX, initialCenterY, 1)
        await new Promise((r) => setTimeout(r, 150))
        await getMap()?.animateTo(initialCenterX, initialCenterY, 0.8)
        updateHashLink({
          xt: lastSubmission.xt,
          yt: lastSubmission.yt,
        })
        isMovingToSubmission.current = false
      }
    }
    void doSubmitAnimate()
  }, [lastSubmission, initialCenterX, initialCenterY, updateHashLink])

  const updateHashLinkFromPosition = useCallback(
    ({ includeVersion }: { includeVersion?: boolean } = {}) => {
      const map = getMap()
      if (!map) {
        return
      }

      const [x, y] = map.getPosition()
      const xt = Math.floor(x / tileWidth)
      const yt = Math.floor(y / tileHeight)

      if (xt < 0 || xt > tilesX - 1 || yt < 0 || yt > tilesY - 1) {
        return
      }

      const nextHashLink: MetaMachineLink = { xt, yt }
      if (includeVersion) {
        nextHashLink.v = metaMachine.version
      }
      updateHashLink(nextHashLink)
    },
    [
      metaMachine.version,
      tileHeight,
      tileWidth,
      tilesX,
      tilesY,
      updateHashLink,
    ],
  )

  const handlePosition = useCallback(
    (viewBounds: Bounds) => {
      onPosition?.(viewBounds)

      if (
        getMap()?.isAnimating() ||
        isMovingToSubmission.current ||
        !isActive
      ) {
        return
      }
      updateHashLinkFromPosition()
    },
    [onPosition, isActive, updateHashLinkFromPosition],
  )

  useEffect(() => {
    const map = getMap()
    if (!hashLink || !map) {
      return
    }
    const [x, y] = map.getPosition()
    const xt = Math.floor(x / tileWidth)
    const yt = Math.floor(y / tileHeight)
    if (hashLink.xt === xt && hashLink.yt === yt) {
      return
    }
    getMachine()?.unfollowBall()
    void map.animateTo(
      hashLink.xt * tileWidth + tileWidth / 2,
      hashLink.yt * tileHeight + tileHeight / 2,
      0.8,
    )
  }, [hashLink, tileHeight, tileWidth])

  const [hasCopied, setHasCopied] = useState(false)

  const handleClickPermalink = useCallback(() => {
    if (!hashLink) {
      return
    }
    async function doCopy() {
      updateHashLinkFromPosition({ includeVersion: true })
      await navigator.clipboard?.writeText(window.location.toString())
      setHasCopied(true)
      await new Promise((r) => setTimeout(r, 1000))
      setHasCopied(false)
    }
    void doCopy()
  }, [hashLink, updateHashLinkFromPosition])

  // Ball follow mode
  const startFollowingBall = useCallback(() => {
    const map = getMap()
    const machine = getMachine()
    if (!map || !machine) {
      return
    }

    map.stop()

    let [lastX, lastY] = map.getPosition()
    machine.followBall((x: number, y: number) => {
      const nextX = lastX * 0.75 + x * 0.25
      const nextY = lastY * 0.75 + y * 0.25
      map.jumpTo(nextX, nextY, 0.8)
      lastX = nextX
      lastY = nextY
    })
  }, [])

  const handleDragStart = useCallback(() => {
    getMachine()?.unfollowBall()
  }, [])

  useEffect(() => {
    function handleKey(ev: KeyboardEvent) {
      if (ev.key.toLowerCase() === 'b' && ev.ctrlKey && ev.altKey) {
        startFollowingBall()
      }
    }
    window.addEventListener('keydown', handleKey, false)
    return () => {
      window.removeEventListener('keydown', handleKey, false)
    }
  }, [startFollowingBall])

  return (
    <>
      <PhysicsContextProvider stepRateMultiplier={isActive ? 1 : 0}>
        <PhysicsLoader spinner={<LoadingSpinner />}>
          <SlippyMetaMachineView
            ref={viewRef}
            {...metaMachine}
            initialX={initialCenterX}
            initialY={initialCenterY}
            initialZoom={0.8}
            onPosition={handlePosition}
            onDragStart={handleDragStart}
          />
        </PhysicsLoader>
      </PhysicsContextProvider>
      <ToolButton
        onClick={startFollowingBall}
        aria-label="Follow a random ball"
        css={[
          {
            position: 'absolute',
            left: 10,
            bottom: 10,
            width: 50,
            height: 50,
            cursor: 'pointer',
          },
          comicDropShadow,
        ]}
      >
        <ComicImage img={imgFollowBall} />
      </ToolButton>
      <ToolButton
        onClick={handleClickPermalink}
        aria-label="Copy permalink"
        css={[
          {
            position: 'absolute',
            left: 10,
            top: 10,
            width: 50,
            height: 50,
            cursor: 'pointer',
          },
          comicDropShadow,
        ]}
      >
        <ComicImage img={imgPermalink} />
      </ToolButton>
      <AnimatePresence>
        {hasCopied && (
          <motion.div
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0, transition: { delay: 1 } }}
            css={[
              {
                position: 'absolute',
                left: 68,
                top: 10,
                fontFamily: 'xkcd-Regular-v3',
                background: 'black',
                color: 'white',
                padding: '4px 8px',
              },
              comicDropShadow,
            ]}
          >
            Copied!
          </motion.div>
        )}
      </AnimatePresence>
    </>
  )
}
