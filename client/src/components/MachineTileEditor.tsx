import imgSubmit from '@art/submit_4x.png'
import imgWrench from '@art/wrench_4x.png'
import { PropsOf } from '@emotion/react'
import useLatest from '@react-hook/latest'
import { motion } from 'framer-motion'
import { max } from 'lodash'
import React, {
  KeyboardEvent,
  useCallback,
  useEffect,
  useRef,
  useState,
} from 'react'
import Moveable, {
  OnDrag,
  OnResize,
  OnResizeStart,
  OnRotate,
} from 'react-moveable'
import { px, useIdGen } from '../lib/utils'
import { Puzzle, WidgetCollection } from '../types'
import { ComicImage } from './ComicImage'
import DebugOverlay from './DebugOverlay'
import { EditorTutorials, useTutorials } from './EditorTutorials'
import { useDisplayState } from './FullscreenComicContainer'
import { useMachine } from './MachineContext'
import { useMachineTile } from './MachineTileContext'
import { useLoopHandler } from './PhysicsContext'
import WidgetPalette, { ToolButton, comicDropShadow } from './WidgetPalette'
import { MAX_WIDGET_COUNT } from './constants'
import { getPositionStyles } from './positionStyles'
import {
  PaletteItem,
  WidgetData,
  Widgets,
  stickerList,
  widgetList,
} from './widgets'
import { Balls } from './widgets/Balls'
import MachineFrame from './widgets/MachineFrame'
import { getNextWheelSpeed } from './widgets/Wheel'

const DEG_TO_RAD = Math.PI / 180

export interface EditableWidget {
  id: string
  onSelect?: (
    ev: React.MouseEvent<HTMLDivElement> | React.TouchEvent<HTMLDivElement>,
    id: string,
  ) => void
  isSelected?: boolean
}

interface Selection {
  id: string
  el: HTMLDivElement
}

export function useSelectHandlers(
  id: EditableWidget['id'],
  onSelect: EditableWidget['onSelect'],
) {
  const handleSelect = useCallback(
    (
      ev: React.MouseEvent<HTMLDivElement> | React.TouchEvent<HTMLDivElement>,
    ) => {
      onSelect?.(ev, id)
    },
    [id, onSelect],
  )
  return { onMouseDown: handleSelect, onTouchStart: handleSelect }
}

type Widgets = Record<string, WidgetData>

// use arrow keys for fun and profit
// ... but only when a wheel is selected
export function useWheelSpeed(
  setWidgets: (setState: (widgets: Widgets) => Widgets) => void,
  selection: Selection | undefined,
) {
  // no advantage to recreating everything each time selection changes imo
  const selectionRef = useRef(selection)
  selectionRef.current = selection
  useEffect(() => {
    function handleKey(ev: KeyboardEvent) {
      const key = ev.key.toLowerCase()
      const { current: selection } = selectionRef
      if (selection == null) {
        return
      }
      setWidgets((widgets) => {
        const selectedWheel = widgets[selection.id]
        if (selectedWheel.type === 'spokedwheel') {
          const speed = getNextWheelSpeed(selectedWheel, key)
          if (speed != null) {
            return {
              ...widgets,
              [selection.id]: {
                ...selectedWheel,
                speed,
              },
            }
          }
        }
        return widgets
      })
    }

    // @ts-expect-error typescript doesn't like going from event to KeyboardEvent or something
    window.addEventListener('keydown', handleKey, false)
    return () => {
      // @ts-expect-error typescript doesn't like going from event to KeyboardEvent or something
      window.removeEventListener('keydown', handleKey, false)
    }
  }, [selectionRef, setWidgets])
}

const dragBounds: PropsOf<typeof Moveable>['bounds'] = {
  left: 0,
  top: 0,
  right: 0,
  bottom: 0,
  position: 'css',
}

export default function MachineTileEditor({
  puzzle,
  initialWidgets,
  onSubmit,
}: {
  puzzle: Puzzle
  initialWidgets: WidgetCollection
  onSubmit?: (widgets: WidgetCollection) => void
}) {
  const { clearBalls, events: machineEvents } = useMachine()
  const { width, height } = useMachineTile()
  const display = useDisplayState()

  const isMobilePalette =
    display.isFullscreen && display.orientation === 'portrait'

  const moveableRef = useRef<Moveable>(null)
  const [isShowingPalette, setShowingPalette] = useState(false)
  const [isManipulating, setManipulating] = useState(false)
  const [selection, setSelection] = useState<Selection>()
  const [isValidOutputs, setValidOutputs] = useState(false)

  const nextId = useIdGen(
    () => max(Object.keys(initialWidgets).map(Number)) ?? 0,
  )
  const [widgets, setWidgets] =
    useState<Record<string, WidgetData>>(initialWidgets)
  const latestWidgets = useLatest(widgets)
  const widgetCount = Object.keys(widgets).length

  const tutorials = useTutorials()
  const canSubmit = isValidOutputs && widgetCount <= MAX_WIDGET_COUNT

  useEffect(() => {
    function showExpiryTutorial() {
      tutorials.showTutorial('expiry')
    }
    function showRoutedTutorial() {
      tutorials.showTutorial('routed')
    }
    machineEvents.on('ballExpired', showExpiryTutorial)
    machineEvents.on('exitBall', showRoutedTutorial)
    return () => {
      machineEvents.off('ballExpired', showExpiryTutorial)
      machineEvents.off('exitBall', showRoutedTutorial)
    }
  }, [machineEvents, tutorials])

  useEffect(() => {
    if (canSubmit) {
      tutorials.showTutorial('submit')
    }
  }, [canSubmit, tutorials])

  const handleStartManipulating = useCallback(() => {
    setManipulating(true)
  }, [])

  const handleEndManipulating = useCallback(() => {
    setManipulating(false)
  }, [])

  useLoopHandler(() => {
    const { current: moveable } = moveableRef
    if (!moveable) {
      return
    }

    setTimeout(() => {
      moveable.updateRect()
    }, 0)
  }, [])

  const applyImmediateStyles = useCallback(
    (
      selection: Selection,
      { x, y, angle }: { x?: number; y?: number; angle?: number },
    ) => {
      const curWidget = latestWidgets.current[selection.id]

      // Not all widgets have rotation so we have to do some type checks for ts to be happy with this.
      const curAngle = 'angle' in curWidget ? curWidget.angle : 0

      Object.assign(
        selection.el,
        getPositionStyles(
          x ?? curWidget.x,
          y ?? curWidget.y,
          angle ?? curAngle,
        ),
      )
    },
    [latestWidgets],
  )

  const handleSelect = useCallback(
    (
      ev: React.MouseEvent<HTMLDivElement> | React.TouchEvent<HTMLDivElement>,
      id: string,
    ) => {
      async function doSelect() {
        const { current: moveable } = moveableRef
        if (!moveable) {
          return
        }

        setSelection({ id, el: ev.currentTarget })
        await moveable.waitToChangeTarget()
        moveable.dragStart(ev.nativeEvent)
      }
      void doSelect()
    },
    [setSelection],
  )

  const handleDrag = useCallback(
    ({ translate: [x, y] }: OnDrag) => {
      if (!selection) {
        return
      }

      applyImmediateStyles(selection, { x, y })

      setWidgets((curWidgets) => {
        const selectedWidget = curWidgets[selection.id]
        return {
          ...curWidgets,
          [selection.id]: {
            ...selectedWidget,
            x,
            y,
          },
        }
      })
    },
    [applyImmediateStyles, selection],
  )

  const handleRotate = useCallback(
    ({ rotation: degRotation }: OnRotate) => {
      if (!selection) {
        return
      }

      const angle = degRotation * DEG_TO_RAD

      applyImmediateStyles(selection, { angle })

      setWidgets((curWidgets) => {
        const selectedWidget = curWidgets[selection.id]
        return {
          ...curWidgets,
          [selection.id]: {
            ...selectedWidget,
            angle,
          },
        }
      })
    },
    [applyImmediateStyles, selection],
  )

  const handleResize = useCallback(
    ({
      target,
      width,
      height,
      drag: {
        translate: [x, y],
        transform,
      },
    }: OnResize) => {
      if (!selection) {
        return
      }

      // It's necessary to immediately apply the styles, otherwise react-moveable gets stale values and glitches out.
      target.style.transform = transform
      target.style.width = px(width)
      target.style.height = px(height)

      setWidgets((curWidgets) => {
        const selectedWidget = curWidgets[selection.id]
        return {
          ...curWidgets,
          [selection.id]: {
            ...selectedWidget,
            x,
            y,
            width,
            height,
          },
        }
      })
    },
    [selection],
  )

  const handleResizeStart = useCallback((ev: OnResizeStart) => {
    ev.setMin([25, 25])
    setManipulating(true)
  }, [])

  const handleDeselect = useCallback((ev: React.MouseEvent<HTMLDivElement>) => {
    if (ev.target === ev.currentTarget) {
      setSelection(undefined)
      setShowingPalette(false)
    }
  }, [])

  const handleOpenPalette = useCallback(() => {
    setShowingPalette(true)
  }, [])

  const handleAddWidget = useCallback(
    (create: PaletteItem<WidgetData>['create']) => {
      setWidgets((curWidgets) => ({
        ...curWidgets,
        [nextId()]: create(width / 2, height / 2),
      }))
    },
    [height, nextId, width],
  )

  const handleTrashWidget = useCallback(() => {
    if (!selection) {
      return
    }

    setWidgets(({ [selection.id]: _removed, ...curWidgets }) => curWidgets)
    setSelection(undefined)
  }, [selection])

  const [widgetsKey, setWidgetsKey] = useState(0)
  const handleEmergencyStop = useCallback(() => {
    // Remove and recreate widgets
    setWidgetsKey((x) => x + 1)
    clearBalls()
  }, [clearBalls])

  const handleSubmit = useCallback(() => {
    onSubmit?.(widgets)
  }, [onSubmit, widgets])

  const [showDebugOverlay, setShowDebugOverlay] = useState(false)
  // Delete key trashes widgets and ctrl+option+shift+d shows the debug overlay
  useEffect(() => {
    function handleKey(ev: KeyboardEvent) {
      if (ev.key === 'Delete') {
        handleTrashWidget()
      } else if (
        ev.key.toLowerCase() === 'd' &&
        ev.shiftKey &&
        ev.ctrlKey &&
        ev.metaKey
      ) {
        setShowDebugOverlay((prev) => !prev)
      } else if (ev.key === 'Escape') {
        setShowDebugOverlay(false)
      }
    }
    // @ts-expect-error typescript doesn't like going from event to KeyboardEvent or something
    window.addEventListener('keydown', handleKey, false)
    return () => {
      // @ts-expect-error typescript doesn't like going from event to KeyboardEvent or something
      window.removeEventListener('keydown', handleKey, false)
    }
  }, [handleTrashWidget, selection, setShowDebugOverlay])

  useWheelSpeed(setWidgets, selection)

  const selectedWidget = selection ? widgets[selection.id] : null
  const selectedWidgetInfo =
    selectedWidget == null
      ? null
      : selectedWidget.type === 'sticker'
        ? stickerList[selectedWidget.sticker]
        : widgetList[selectedWidget.type]

  return (
    <div
      onMouseDown={handleDeselect}
      css={{
        position: 'relative',
        width,
        height,
        overflow: 'hidden',
        userSelect: 'none',
      }}
    >
      <EditorTutorials
        visibleTutorial={tutorials.visibleTutorial}
        onDismissTutorial={tutorials.dismissTutorial}
      />
      {/* portal so can display outside extents? */}
      <Moveable
        ref={moveableRef}
        css={{
          '.moveable-control.moveable-origin': {
            display: 'none',
          },
        }}
        target={selection?.el}
        onDrag={handleDrag}
        onRotate={handleRotate}
        onResize={handleResize}
        onResizeStart={handleResizeStart}
        onDragStart={handleStartManipulating}
        onDragEnd={handleEndManipulating}
        onRotateStart={handleStartManipulating}
        onRotateEnd={handleEndManipulating}
        onResizeEnd={handleEndManipulating}
        keepRatio={selectedWidgetInfo?.isSquare}
        rotatable={selectedWidgetInfo?.canRotate}
        resizable={selectedWidgetInfo?.canResize}
        bounds={dragBounds}
        draggable
        snappable
      />
      <Widgets
        key={widgetsKey}
        widgets={widgets}
        onSelect={handleSelect}
        selectedId={selection?.id}
      />
      <Balls />
      <motion.div
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ duration: 1 }}
      >
        <MachineFrame
          inputs={puzzle.inputs}
          outputs={puzzle.outputs}
          onValidate={setValidOutputs}
          spawnBallsTop
          spawnBallsLeft
          spawnBallsRight
          validateOutputs
        />
      </motion.div>
      <motion.div
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.5, duration: 1 }}
      >
        {!isMobilePalette && (
          <ToolButton
            onClick={handleOpenPalette}
            css={[
              {
                position: 'absolute',
                right: 10,
                top: 10,
                opacity: isShowingPalette ? 0 : 1,
                transition: 'opacity 100ms ease-out',
              },
              comicDropShadow,
            ]}
            aria-label="Toolbox"
          >
            <ComicImage img={imgWrench} />
          </ToolButton>
        )}
        <ToolButton
          initial={false}
          animate={{
            scale:
              tutorials.seenTutorials.submit ||
              tutorials.visibleTutorial === 'submit'
                ? 1
                : 0,
            opacity: canSubmit ? 1 : 0.5,
          }}
          onClick={handleSubmit}
          disabled={!canSubmit}
          aria-label="Submit your blueprint"
          css={[
            {
              position: 'absolute',
              left: 10,
              top: 10,
              width: 50,
              height: 50,
              cursor: canSubmit ? 'pointer' : 'not-allowed',
            },
            comicDropShadow,
          ]}
        >
          <ComicImage img={imgSubmit} />
        </ToolButton>
      </motion.div>
      <WidgetPalette
        onAdd={handleAddWidget}
        onTrash={handleTrashWidget}
        onEmergencyStop={handleEmergencyStop}
        widgetCount={widgetCount}
        isHorizontal={isMobilePalette}
        css={
          isMobilePalette
            ? {
                position: 'fixed',
                bottom: -116,
                height: 100,
                left: 8,
                right: 8,
              }
            : {
                position: 'absolute',
                right: 10,
                top: 10,
                bottom: 10,
                width: 72,
                transform: isShowingPalette
                  ? ''
                  : `translateX(calc(100% + 10px))`,
                opacity: isManipulating ? 0.1 : 1,
                transition: 'transform 250ms ease, opacity 100ms ease-out',
                pointerEvents: isManipulating ? 'none' : 'all',
              }
        }
      />
      {showDebugOverlay && <DebugOverlay />}
    </div>
  )
}
