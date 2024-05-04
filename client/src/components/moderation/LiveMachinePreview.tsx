import imgCheck from '@art/check-circle_4x.png'
import imgWrong from '@art/wrong-circle_4x.png'
import {
  ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react'
import { Bounds, Puzzle } from '../../types'
import { ComicImage } from '../ComicImage'
import LoadingSpinner from '../LoadingSpinner'
import {
  MachineContextProvider,
  MachineContextProviderRef,
} from '../MachineContext'
import {
  MachineTileContextProvider,
  MachineTileContextProviderRef,
} from '../MachineTileContext'
import { PhysicsContextProvider } from '../PhysicsContext'
import { MAX_WIDGET_COUNT } from '../constants'
import { CircleGauge } from '../widgets/CircleGauge'
import ModMachineTileView from './ModMachineTileView'
import { ModLocation, ModMachine, ServerBlueprint } from './modTypes'
import {
  useApproveBlueprint,
  useBurnBlueprint,
  useReissuePuzzle,
} from './moderatorClient'

const MIN_SECONDS_TO_MOD = 30

function useCountdown(initialSeconds: number) {
  const [seconds, setSeconds] = useState(initialSeconds)

  useEffect(() => {
    const startTime = performance.now()

    function dec() {
      const now = performance.now()
      const remainingSeconds = initialSeconds - (now - startTime) / 1000
      setSeconds(remainingSeconds)
      if (remainingSeconds <= 0) {
        clearInterval(interval)
      }
    }
    const interval = setInterval(dec, 150)
    return () => {
      clearInterval(interval)
    }
  }, [initialSeconds])

  return seconds
}

function ValidationLine({
  isValid,
  children,
}: {
  isValid: boolean
  children: ReactNode
}) {
  return (
    <div
      css={{
        display: 'flex',
        alignItems: 'center',
        gap: 8,
      }}
    >
      <ComicImage img={isValid ? imgCheck : imgWrong} />
      <span>{children}</span>
    </div>
  )
}

export function LiveMachinePreview({
  loc,
  modMachine,
  puzzle,
  blueprintId,
  blueprint,
  onNextBlueprint,
}: {
  loc: ModLocation
  modMachine: ModMachine
  puzzle: Puzzle
  blueprintId: string | undefined
  blueprint: ServerBlueprint | undefined
  onNextBlueprint: () => void
}) {
  const machineRef = useRef<MachineContextProviderRef>(null)
  const machineTileRef = useRef<MachineTileContextProviderRef>(null)

  const approveBlueprint = useApproveBlueprint()
  const burnBlueprint = useBurnBlueprint()
  const reissuePuzzle = useReissuePuzzle()

  const actionCountdown = useCountdown(MIN_SECONDS_TO_MOD)

  const [isValidOutputs, setValidOutputs] = useState(false)
  const widgetCount = blueprint ? Object.keys(blueprint.widgets).length : 0
  const isWidgetCountValid = widgetCount <= MAX_WIDGET_COUNT
  const canApprove =
    actionCountdown <= 0 && isValidOutputs && isWidgetCountValid

  const handleOutputValidate = useCallback((isValid: boolean) => {
    setValidOutputs(isValid)
  }, [])

  let modStatus = ''
  const isError =
    approveBlueprint.isError || burnBlueprint.isError || reissuePuzzle.isError
  if (approveBlueprint.isPending || burnBlueprint.isPending) {
    modStatus = 'Working...'
  } else if (isError) {
    modStatus = 'Error :('
  } else if (approveBlueprint.isSuccess) {
    modStatus = 'Approved!'
  } else if (burnBlueprint.isSuccess) {
    modStatus = 'Burnt!'
  } else if (reissuePuzzle.isSuccess) {
    modStatus = 'Reissued!'
  }

  const handleApprove = useCallback(() => {
    const { current: machineTile } = machineTileRef
    if (!machineTile || !blueprintId) {
      return
    }
    const snapshot = machineTile.snapshot()
    approveBlueprint.mutate({
      xt: loc.xt,
      yt: loc.yt,
      blueprintId,
      snapshot,
    })
  }, [approveBlueprint, blueprintId, loc.xt, loc.yt])

  const handleBurn = useCallback(() => {
    if (!blueprintId) {
      return
    }
    void burnBlueprint.mutate({ puzzleId: loc.puzzle, blueprintId })
    onNextBlueprint()
  }, [blueprintId, burnBlueprint, loc.puzzle, onNextBlueprint])

  const handleReissue = useCallback(() => {
    if (!blueprint) {
      return
    }
    void reissuePuzzle.mutate({ puzzleId: blueprint.puzzle })
  }, [blueprint, reissuePuzzle])

  const tileBounds: Bounds = useMemo(
    () => [0, 0, modMachine.tile_size.x, modMachine.tile_size.y],
    [modMachine.tile_size.x, modMachine.tile_size.y],
  )

  return (
    <div
      css={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        gap: 16,
      }}
    >
      <PhysicsContextProvider>
        <MachineContextProvider
          ref={machineRef}
          msPerBall={modMachine.ms_per_ball}
          initialSimulationBounds={tileBounds}
          initialViewBounds={tileBounds}
        >
          <MachineTileContextProvider ref={machineTileRef} bounds={tileBounds}>
            {!puzzle ? (
              <LoadingSpinner />
            ) : (
              <ModMachineTileView
                puzzle={puzzle}
                blueprint={blueprint}
                width={500}
                height={500}
                tileWidth={modMachine.tile_size.x}
                tileHeight={modMachine.tile_size.y}
                onValidate={handleOutputValidate}
              />
            )}
          </MachineTileContextProvider>
        </MachineContextProvider>
      </PhysicsContextProvider>
      {blueprint ? (
        <>
          <div css={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
            <h3 css={{ margin: 0 }}>&quot;{blueprint.title}&quot;</h3>
            <ValidationLine isValid={isValidOutputs}>
              {isValidOutputs
                ? 'All outputs are valid'
                : 'Not all outputs are valid'}
            </ValidationLine>
            <ValidationLine isValid={isWidgetCountValid}>
              {widgetCount} / {MAX_WIDGET_COUNT} widgets used
            </ValidationLine>
          </div>
          <div
            css={{
              display: 'flex',
              flexDirection: 'row',
              flexWrap: 'wrap',
              gap: 8,
            }}
          >
            <CircleGauge
              value={actionCountdown / 30}
              lineWidth={0.5}
              css={{
                width: 20,
                stroke: 'gray',
              }}
            />
            <button onClick={handleApprove} disabled={!canApprove}>
              Approve
            </button>
            <button onClick={handleBurn}>Burn</button>
            <button onClick={handleReissue}>Reissue Puzzle</button>
            {modStatus && (
              <span style={{ color: isError ? 'red' : 'black' }}>
                {modStatus}
              </span>
            )}
          </div>
        </>
      ) : null}
    </div>
  )
}
