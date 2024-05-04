import imgEdit from '@art/edit-icon_4x.png'
import imgView from '@art/view-icon_4x.png'
import { motion } from 'framer-motion'
import { isEqual, random, throttle } from 'lodash'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import comic from '../../comic.json'
import { Bounds } from '../types'
import { ComicBrowseView } from './ComicBrowseView'
import { ComicImage } from './ComicImage'
import { ComicPuzzleView } from './ComicPuzzleView'
import { FullscreenComicContainer } from './FullscreenComicContainer'
import LoadingSpinner from './LoadingSpinner'
import { ToolButton, comicDropShadow } from './WidgetPalette'
import { paramToNumber, useLocationHashParams } from './useLocationHashParams'
import {
  SavedMachine,
  useGetPuzzles,
  useMetaMachineClient,
} from './useMetaMachineClient'

export interface MetaMachineLink {
  xt: number
  yt: number
  v?: number
}

export function useHashLink(): {
  hashLink: MetaMachineLink | null
  updateHashLink: (link: MetaMachineLink) => void
} {
  const { locationHashParams, setLocationHashParams } = useLocationHashParams()
  const lastRef = useRef<MetaMachineLink | null>(null)

  const hashLink = useMemo(() => {
    const xt = paramToNumber(locationHashParams.get('xt'))
    const yt = paramToNumber(locationHashParams.get('yt'))
    const v = paramToNumber(locationHashParams.get('v'))
    if (xt == null || yt == null) {
      return null
    }
    const nextVal = {
      xt,
      yt,
      v,
    }
    lastRef.current = nextVal
    return nextVal
  }, [locationHashParams])

  const updateHashLink = useCallback(
    (update: MetaMachineLink) => {
      const nextVal = { ...lastRef.current, ...update }
      if (isEqual(nextVal, lastRef.current)) {
        return
      }
      lastRef.current = nextVal
      const { xt, yt, v } = nextVal
      setLocationHashParams({
        xt: String(xt),
        yt: String(yt),
        v: v != null ? String(v) : undefined,
      })
    },
    [setLocationHashParams],
  )

  return { hashLink, updateHashLink }
}

export default function Comic() {
  const { hashLink, updateHashLink } = useHashLink()
  const [mode, setMode] = useState<'create' | 'browse'>(
    hashLink ? 'browse' : 'create',
  )

  const [viewBounds, setViewBounds] = useState<Bounds>(() => [
    0,
    0,
    comic.width,
    comic.height,
  ])

  const throttledSetViewBounds = useMemo(() => throttle(setViewBounds, 250), [])

  const [lastSubmission, setLastSubmission] = useState<SavedMachine>()
  const { metaMachine, allTilesLoaded } = useMetaMachineClient({
    viewBounds,
    lastSubmission,
    version: hashLink?.v,
  })

  const [isInitialLoading, setInitialLoading] = useState(true)
  useEffect(() => {
    if (allTilesLoaded) {
      setInitialLoading(false)
    }
  }, [allTilesLoaded])

  const puzzles = useGetPuzzles()

  // TODO: track seen puzzles
  const [puzzleIdx, setPuzzleIdx] = useState<number>(0)
  useEffect(() => {
    if (!puzzles) {
      return
    }
    setPuzzleIdx(random(puzzles.length - 1))
  }, [puzzles])

  const handleSubmitBlueprint = useCallback((location: SavedMachine) => {
    setLastSubmission(location)
    setMode('browse')
  }, [])

  const handleToggleMode = useCallback(() => {
    setMode((curMode) => (curMode === 'create' ? 'browse' : 'create'))
  }, [])

  if (!metaMachine) {
    return <LoadingSpinner />
  }

  const puzzle = puzzles ? puzzles[puzzleIdx] : undefined

  return (
    <FullscreenComicContainer>
      <motion.div
        initial={{ opacity: 0 }}
        animate={{ opacity: isInitialLoading ? 0 : 1 }}
        transition={{ delay: 0.25 }}
        css={{ contain: 'paint' }}
      >
        <ComicBrowseView
          metaMachine={metaMachine}
          lastSubmission={lastSubmission}
          onPosition={throttledSetViewBounds}
          hashLink={hashLink}
          updateHashLink={updateHashLink}
          isActive={mode === 'browse'}
        />
      </motion.div>
      {puzzle && (
        <motion.div
          css={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: '100%',
            height: '100%',
            backgroundColor: 'white',
          }}
          initial={{ opacity: mode === 'create' ? 1 : 0 }}
          animate={{
            opacity: mode === 'create' ? 1 : 0,
            scale: mode === 'create' ? 1 : 0.9,
          }}
          transition={{ type: 'spring', duration: 0.25 }}
          style={{
            pointerEvents: mode === 'create' && puzzle ? 'all' : 'none',
          }}
        >
          <ComicPuzzleView
            key={`${puzzle.id}-${lastSubmission?.blueprintId}`}
            puzzle={puzzle}
            metaMachine={metaMachine}
            onSubmit={handleSubmitBlueprint}
            isActive={mode === 'create'}
          />
        </motion.div>
      )}
      {puzzle && (
        <ToolButton
          initial={{ scale: 0 }}
          animate={
            mode === 'create' ? { scale: 1 } : { scale: 1, rotateY: 180 }
          }
          transition={isInitialLoading ? { scale: { delay: 0.5 } } : undefined}
          onClick={handleToggleMode}
          aria-label={
            mode === 'browse' ? 'Build another blueprint' : 'View the machine'
          }
          css={{
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            position: 'absolute',
            right: 10,
            bottom: 10,
            width: 50,
            height: 50,
            transformStyle: 'preserve-3d',
          }}
        >
          <ComicImage
            img={imgEdit}
            css={[
              {
                position: 'absolute',
                transform: 'rotateY(180deg) translateZ(0.01px)',
                backfaceVisibility: 'hidden',
              },
              comicDropShadow,
            ]}
          />
          <ComicImage
            img={imgView}
            css={[
              {
                backfaceVisibility: 'hidden',
              },
              comicDropShadow,
            ]}
          />
        </ToolButton>
      )}
    </FullscreenComicContainer>
  )
}
