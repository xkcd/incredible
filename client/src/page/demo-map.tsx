import { StrictMode, useEffect, useRef } from 'react'
import { createRoot } from 'react-dom/client'
import { SlippyMapRef } from '../components/CenteredSlippyMap'
import InnerComicBorder from '../components/InnerComicBorder'
import { SlippyMetaMachineView } from '../components/MetaMachineView'
import { PhysicsContextProvider } from '../components/PhysicsContext'
import { emptyPuzzle } from './fixtures/emptyMachine'

const getMachine = () => ({
  puzzle: emptyPuzzle,
  widgets: {},
  snapshot: { widgets: {}, balls: [] },
})

function DemoMap({ demoPanning }: { demoPanning?: boolean }) {
  const mapRef = useRef<SlippyMapRef>(null)

  useEffect(() => {
    if (!demoPanning) {
      return
    }
    const interval = setInterval(() => {
      void mapRef.current?.animateTo(
        1000 + 1000 * Math.random(),
        1000 + 1000 * Math.random(),
        0.25 + Math.random(),
      )
    }, 1000)
    return () => {
      clearInterval(interval)
    }
  }, [demoPanning])

  return (
    <SlippyMetaMachineView
      ref={mapRef}
      getMachine={getMachine}
      tilesX={100}
      tilesY={100}
      tileWidth={740}
      tileHeight={740}
      initialX={370}
      initialY={370}
      initialZoom={1}
      msPerBall={1000}
    />
  )
}

const root = createRoot(document.getElementsByTagName('main')[0])
root.render(
  <StrictMode>
    <PhysicsContextProvider>
      <InnerComicBorder>
        <DemoMap />
      </InnerComicBorder>
    </PhysicsContextProvider>
  </StrictMode>,
)
