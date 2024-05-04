import { QueryClientProvider } from '@tanstack/react-query'
import { StrictMode, useState } from 'react'
import { createRoot } from 'react-dom/client'
import comic from '../../comic.json'
import { queryClient } from '../api'
import InnerComicBorder from '../components/InnerComicBorder'
import LoadingSpinner from '../components/LoadingSpinner'
import { SlippyMetaMachineView } from '../components/MetaMachineView'
import { PhysicsContextProvider } from '../components/PhysicsContext'
import { useMetaMachineClient } from '../components/useMetaMachineClient'
import { Bounds } from '../types'

function DemoViewer() {
  const [viewBounds, setViewBounds] = useState<Bounds>(() => [
    0,
    0,
    comic.width,
    comic.height,
  ])

  const { metaMachine } = useMetaMachineClient({ viewBounds })

  /*
  const clippedMetaMachine = useMemo(
    () =>
      metaMachine
        ? {
            ...metaMachine,
            tilesY: 1,
          }
        : null,
    [metaMachine],
  )
  */

  if (!metaMachine) {
    return <LoadingSpinner />
  }

  return (
    <SlippyMetaMachineView
      {...metaMachine}
      initialX={metaMachine.tileWidth / 2}
      initialY={metaMachine.tileHeight / 2}
      initialZoom={0.8}
      onPosition={setViewBounds}
    />
  )
}

const root = createRoot(document.getElementsByTagName('main')[0])
root.render(
  <StrictMode>
    <QueryClientProvider client={queryClient}>
      <PhysicsContextProvider>
        <InnerComicBorder>
          <DemoViewer />
        </InnerComicBorder>
      </PhysicsContextProvider>
    </QueryClientProvider>
  </StrictMode>,
)
