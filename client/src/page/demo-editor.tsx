import { StrictMode, useRef, useState } from 'react'
import { createRoot } from 'react-dom/client'
import comic from '../../comic.json'
import InnerComicBorder from '../components/InnerComicBorder'
import {
  MachineContextProvider,
  MachineContextProviderRef,
} from '../components/MachineContext'
import {
  MachineTileContextProvider,
  MachineTileContextProviderRef,
} from '../components/MachineTileContext'
import MachineTileEditor from '../components/MachineTileEditor'
import { PhysicsContextProvider } from '../components/PhysicsContext'
import { MachineSnapshot } from '../lib/snapshot'
import { Bounds } from '../types'
import { emptyPuzzle, emptyWidgets } from './fixtures/emptyMachine'

const comicBounds: Bounds = [0, 0, comic.width, comic.height]

function DemoEditor() {
  const machineRef = useRef<MachineContextProviderRef>()
  const machineTileRef = useRef<MachineTileContextProviderRef>()
  const [snapshot, setSnapshot] = useState<MachineSnapshot | undefined>()

  return (
    <div css={{ width: '100%', height: '100%' }}>
      <PhysicsContextProvider debug>
        <MachineContextProvider
          ref={machineRef}
          initialSimulationBounds={comicBounds}
          initialViewBounds={comicBounds}
          msPerBall={1000}
        >
          <MachineTileContextProvider
            ref={machineTileRef}
            bounds={[0, 0, comic.width, comic.height]}
          >
            <MachineTileEditor
              puzzle={emptyPuzzle}
              initialWidgets={emptyWidgets}
            />
          </MachineTileContextProvider>
        </MachineContextProvider>
      </PhysicsContextProvider>
      <button
        onClick={() => {
          const nextSnapshot = machineTileRef.current?.snapshot()
          setSnapshot(nextSnapshot)
          console.log(nextSnapshot)
        }}
      >
        snapshot
      </button>
      <button
        onClick={() => {
          if (!snapshot) {
            return
          }
          machineRef.current?.clearBalls()
          machineTileRef.current?.loadSnapshot(snapshot)
        }}
      >
        load
      </button>
    </div>
  )
}

const root = createRoot(document.getElementsByTagName('main')[0])
root.render(
  <StrictMode>
    <InnerComicBorder>
      <DemoEditor />
    </InnerComicBorder>
  </StrictMode>,
)
