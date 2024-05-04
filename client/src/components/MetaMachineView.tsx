import { ClassNames } from '@emotion/react'
import { throttle } from 'lodash'
import React, {
  RefObject,
  forwardRef,
  useEffect,
  useImperativeHandle,
  useMemo,
  useRef,
  useState,
} from 'react'
import { MachineSnapshot } from '../lib/snapshot'
import { gridViewBounds, iterTiles, tileKey } from '../lib/tiles'
import { Bounds, Puzzle, WidgetCollection } from '../types'
import { BallPitMechanism } from './BallPitMechanism'
import {
  CenteredSlippyMap,
  CenteredSlippyMapProps,
  SlippyMapRef,
} from './CenteredSlippyMap'
import {
  MachineContextProvider,
  MachineContextProviderRef,
} from './MachineContext'
import { MachineTileContextProvider } from './MachineTileContext'
import MachineTilePlaceholder from './MachineTilePlaceholder'
import { deferATick } from './PhysicsContext'
import { BOTTOM_MECHANISM_HEIGHT } from './constants'
import { MetaMachineInfo } from './useMetaMachineClient'
import { Widgets } from './widgets'
import { Balls } from './widgets/Balls'
import MachineFrame from './widgets/MachineFrame'

const MetaMachineTile = React.memo(function MetaMachineTiles({
  puzzle,
  widgets,
  snapshot,
  title,
  tileWidth,
  tileHeight,
  xt,
  yt,
  spawnBallsTop,
  spawnBallsLeft,
  spawnBallsRight,
}: {
  puzzle: Puzzle
  widgets: WidgetCollection
  snapshot: MachineSnapshot | undefined
  title: string | undefined
  tileWidth: number
  tileHeight: number
  xt: number
  yt: number
  spawnBallsTop: boolean
  spawnBallsLeft: boolean
  spawnBallsRight: boolean
}) {
  // Stagger tile renders so we don't have entire rows or columns mounting and updating the world state at the same time.
  const [isRendered, setRendered] = useState(false)
  useEffect(() => {
    function draw() {
      setRendered(true)
    }
    const timeout = deferATick(draw)
    return () => {
      clearTimeout(timeout)
    }
  }, [])

  if (!isRendered) {
    return
  }

  return (
    <MachineTileContextProvider
      initialSnapshot={snapshot}
      bounds={[
        xt * tileWidth,
        yt * tileHeight,
        (xt + 1) * tileWidth,
        (yt + 1) * tileHeight,
      ]}
    >
      <MachineFrame
        title={title}
        inputs={puzzle.inputs}
        outputs={puzzle.outputs}
        spawnBallsTop={spawnBallsTop}
        spawnBallsLeft={spawnBallsLeft}
        spawnBallsRight={spawnBallsRight}
      />
      <Widgets widgets={widgets} />
    </MachineTileContextProvider>
  )
})

export interface SlippyMetaMachineViewProps extends MetaMachineInfo {
  initialX?: number
  initialY?: number
  initialZoom?: number
  simulateOutset?: number
  onPosition?: CenteredSlippyMapProps['onPosition']
  onDragStart?: CenteredSlippyMapProps['onDragStart']
}

export interface SlippyMetaMachineRef {
  mapRef: RefObject<SlippyMapRef | undefined>
  machineRef: RefObject<MachineContextProviderRef | undefined>
}

export const SlippyMetaMachineView = forwardRef<
  SlippyMetaMachineRef,
  SlippyMetaMachineViewProps
>(function SlippyMetaMachineView(
  {
    getMachine,
    tilesX,
    tilesY,
    tileWidth,
    tileHeight,
    msPerBall,
    initialX = 0,
    initialY = 0,
    initialZoom = 1,
    simulateOutset = 150,
    onPosition,
    onDragStart,
  }: SlippyMetaMachineViewProps,
  ref,
) {
  const mapRef = useRef<SlippyMapRef>(null)
  const machineRef = useRef<MachineContextProviderRef>(null)

  const [xt1, setXt1] = useState(0)
  const [yt1, setYt1] = useState(0)
  const [xt2, setXt2] = useState(1)
  const [yt2, setYt2] = useState(1)

  const totalWidth = tileWidth * tilesX
  const totalHeight = tileHeight * tilesY + BOTTOM_MECHANISM_HEIGHT

  const [isBottomVisible, setBottomVisible] = useState(false)

  const updatePosition = useMemo(
    () =>
      throttle((viewBounds: Bounds) => {
        const { current: machine } = machineRef
        if (!machine) {
          return
        }

        machine.viewBoundsRef.current = viewBounds

        const [xt1, yt1, xt2, yt2] = gridViewBounds(
          viewBounds,
          tilesX,
          tilesY,
          tileWidth,
          tileHeight,
          simulateOutset,
        )

        setXt1(xt1)
        setYt1(yt1)
        setXt2(xt2)
        setYt2(yt2)

        machine.simulationBoundsRef.current = [
          xt1 * tileWidth,
          yt1 * tileHeight,
          (xt2 + 1) * tileWidth,
          (yt2 + 1) * tileHeight,
        ]

        onPosition?.(viewBounds)

        const [, , , y2] = viewBounds
        setBottomVisible(y2 > totalHeight - BOTTOM_MECHANISM_HEIGHT)
      }, 1000 / 60),
    [
      tilesX,
      tilesY,
      tileWidth,
      tileHeight,
      simulateOutset,
      onPosition,
      totalHeight,
    ],
  )

  useImperativeHandle(ref, () => ({ mapRef, machineRef }))

  return (
    <ClassNames>
      {({ css }) => (
        <CenteredSlippyMap
          ref={mapRef}
          width={tileWidth}
          height={tileHeight}
          totalWidth={totalWidth}
          totalHeight={totalHeight}
          innerClassName={css({ outline: '1px solid black' })}
          onPosition={updatePosition}
          onDragStart={onDragStart}
          initialX={initialX}
          initialY={initialY}
          initialZoom={initialZoom}
        >
          <MachineContextProvider ref={machineRef} msPerBall={msPerBall}>
            {Array.from(iterTiles(xt1, yt1, xt2, yt2), ([xt, yt]) => {
              const data = getMachine(xt, yt)
              if (!data) {
                return (
                  <MachineTilePlaceholder
                    key={tileKey(xt, yt)}
                    tileWidth={tileWidth}
                    tileHeight={tileHeight}
                    xt={xt}
                    yt={yt}
                  />
                )
              }
              const { blueprintId, title, puzzle, widgets, snapshot } = data
              return (
                <MetaMachineTile
                  key={`${tileKey(xt, yt)}-${blueprintId}`}
                  puzzle={puzzle}
                  widgets={widgets}
                  snapshot={snapshot}
                  title={title}
                  tileWidth={tileWidth}
                  tileHeight={tileHeight}
                  xt={xt}
                  yt={yt}
                  spawnBallsTop={yt === yt1 || !getMachine(xt, yt - 1)}
                  spawnBallsLeft={xt === xt1 || !getMachine(xt - 1, yt)}
                  spawnBallsRight={xt === xt2 || !getMachine(xt + 1, yt)}
                />
              )
            })}
            <BallPitMechanism
              tileWidth={tileWidth}
              tileHeight={tileHeight}
              tilesX={tilesX}
              tilesY={tilesY}
              stepRateMultiplier={isBottomVisible ? 1 : 0.05}
            />
            <Balls />
          </MachineContextProvider>
        </CenteredSlippyMap>
      )}
    </ClassNames>
  )
})
