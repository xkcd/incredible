import { AnimatePresence } from 'framer-motion'
import { useCallback, useMemo, useRef, useState } from 'react'
import invariant from 'tiny-invariant'
import { emptyWidgets } from '../page/fixtures/emptyMachine'
import { Bounds, PuzzleOrder, WidgetCollection } from '../types'
import LoadingSpinner from './LoadingSpinner'
import { MachineContextProvider } from './MachineContext'
import {
  MachineTileContextProvider,
  MachineTileContextProviderRef,
} from './MachineTileContext'
import MachineTileEditor from './MachineTileEditor'
import { NamePrompt } from './NamePrompt'
import { PhysicsContextProvider, PhysicsLoader } from './PhysicsContext'
import {
  MetaMachineInfo,
  SavedMachine,
  useSubmitBlueprint,
} from './useMetaMachineClient'

function saveSolution(location: {
  blueprintId: string
  xt: number
  yt: number
}) {
  try {
    const existingValue = JSON.parse(
      localStorage.getItem('contraptions') ?? '[]',
    ) as Array<unknown>
    localStorage['contraptions'] = JSON.stringify([...existingValue, location])
  } catch (err) {
    console.warn(
      `Submitted ${location.blueprintId}, failed to save to localStorage`,
      err,
    )
  }
}

export function ComicPuzzleView({
  puzzle,
  metaMachine,
  isActive,
  onSubmit,
}: {
  puzzle: PuzzleOrder
  metaMachine: MetaMachineInfo | null
  isActive: boolean
  onSubmit: (location: SavedMachine) => void
}) {
  const machineTileRef = useRef<MachineTileContextProviderRef>()

  const submitBlueprint = useSubmitBlueprint()

  const [isNaming, setNaming] = useState(false)
  const [widgets, setWidgets] = useState<WidgetCollection | null>(null)

  const handleSubmit = useCallback((widgets: WidgetCollection) => {
    setWidgets(widgets)
    setNaming(true)
  }, [])

  const handleCancelName = useCallback(() => {
    setNaming(false)
  }, [])

  const handleSend = useCallback(
    (title: string) => {
      async function doSubmit() {
        invariant(widgets, 'widgets cannot be null')
        invariant(puzzle, 'puzzle cannot be null')

        // TODO: spinner
        const location = await submitBlueprint.mutateAsync({
          puzzleId: puzzle.id,
          workOrder: puzzle.workOrder,
          title,
          widgets,
        })

        if (location) {
          const snapshot = machineTileRef.current?.snapshot()
          onSubmit({ ...location, title, widgets, puzzle, snapshot })
          saveSolution(location)
        }
      }
      void doSubmit()
    },
    [onSubmit, puzzle, submitBlueprint, widgets],
  )

  const tileBounds: Bounds | null = useMemo(
    () =>
      metaMachine
        ? [0, 0, metaMachine.tileWidth, metaMachine.tileHeight]
        : null,
    [metaMachine],
  )

  if (!metaMachine || !puzzle || !tileBounds) {
    return <LoadingSpinner />
  }

  return (
    <PhysicsContextProvider stepRateMultiplier={isActive ? 1 : 0}>
      <PhysicsLoader spinner={<LoadingSpinner />}>
        <MachineContextProvider
          initialSimulationBounds={tileBounds}
          initialViewBounds={tileBounds}
          msPerBall={metaMachine.msPerBall}
        >
          <MachineTileContextProvider ref={machineTileRef} bounds={tileBounds}>
            <AnimatePresence>
              {isNaming ? (
                <NamePrompt onSubmit={handleSend} onCancel={handleCancelName} />
              ) : null}
            </AnimatePresence>
            <MachineTileEditor
              key={puzzle.id}
              puzzle={puzzle}
              initialWidgets={emptyWidgets}
              onSubmit={handleSubmit}
            />
          </MachineTileContextProvider>
        </MachineContextProvider>
      </PhysicsLoader>
    </PhysicsContextProvider>
  )
}
