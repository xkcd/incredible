import imgTutorialCongrats from '@art/tutorial_congrats_4x.png'
import imgTutorialExpiry from '@art/tutorial_expiry_4x.png'
import imgTutorialRouted from '@art/tutorial_routed_4x.png'
import { AnimatePresence } from 'framer-motion'
import { useCallback, useMemo, useState } from 'react'
import { ComicImage } from './ComicImage'
import { SwooshyDialog } from './SwooshyDialog'

const tutorials = {
  routed: imgTutorialRouted,
  expiry: imgTutorialExpiry,
  submit: imgTutorialCongrats,
} as const

const tutorialKeys = Object.keys(tutorials)

type TutorialKey = keyof typeof tutorials
type TutorialState = { [K in TutorialKey]: boolean }

const STORE_KEY = 'contraptionTips'

function loadState(): TutorialState {
  let data: Record<string, boolean> = {}
  try {
    const stored = localStorage.getItem(STORE_KEY)
    data = JSON.parse(stored ?? '{}') as Record<string, boolean>
  } catch {
    // Use defaults
  }

  return Object.fromEntries(
    tutorialKeys.map((key) => [key, Boolean(data[key]) ?? false]),
  ) as TutorialState
}

function saveState(state: TutorialState) {
  localStorage[STORE_KEY] = JSON.stringify(state)
}

export function useTutorials() {
  const [seenTutorials, setSeenTutorials] = useState(loadState)

  const [visibleTutorial, setVisibleTutorial] = useState<TutorialKey | null>(
    null,
  )

  const showTutorial = useCallback(
    (key: TutorialKey) => {
      if (seenTutorials[key] || visibleTutorial != null) {
        return
      }
      setVisibleTutorial(key)
    },
    [seenTutorials, visibleTutorial],
  )

  const dismissTutorial = useCallback(
    (key: TutorialKey) => {
      setVisibleTutorial(null)

      const nextState = { ...seenTutorials, [key]: true }
      setSeenTutorials(nextState)
      saveState(nextState)
    },
    [seenTutorials],
  )

  return useMemo(
    () => ({ seenTutorials, visibleTutorial, showTutorial, dismissTutorial }),
    [dismissTutorial, seenTutorials, showTutorial, visibleTutorial],
  )
}

export function EditorTutorials({
  visibleTutorial,
  onDismissTutorial,
}: {
  visibleTutorial: TutorialKey | null
  onDismissTutorial: (key: TutorialKey) => void
}) {
  const handleDismiss = useCallback(() => {
    if (!visibleTutorial) {
      return
    }
    onDismissTutorial(visibleTutorial)
  }, [onDismissTutorial, visibleTutorial])

  return (
    <AnimatePresence>
      {visibleTutorial ? (
        <SwooshyDialog onDismiss={handleDismiss}>
          <div
            css={{
              display: 'flex',
              position: 'relative',
            }}
          >
            <ComicImage
              img={tutorials[visibleTutorial]}
              css={{
                filter: 'drop-shadow(5px 5px 0 rgba(0, 0, 0, 0.5))',
              }}
            />
            <button
              css={{
                position: 'absolute',
                background: 'none',
                border: 'none',
                top: 0,
                right: 0,
                width: 24,
                height: 24,
                cursor: 'pointer',
              }}
              onClick={handleDismiss}
              aria-label="Close"
            />
          </div>
        </SwooshyDialog>
      ) : null}
    </AnimatePresence>
  )
}
