import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'

import { QueryClientProvider } from '@tanstack/react-query'
import comic from '../comic.json'
import { queryClient } from './api'
import Comic from './components/Comic'

// TODO: comic global
export interface ComicGlobal {}

export function styleContainer(el: HTMLElement) {
  el.style.cssText = `
  position: relative;
  width: ${comic.width}px;
  height: ${comic.height}px;
  margin: 0 auto;
  font-variant: normal;
`
}

function init() {
  const comicEl = document.getElementById('comic')
  if (!comicEl) {
    return
  }

  styleContainer(comicEl)

  const root = createRoot(comicEl)
  root.render(
    <StrictMode>
      <QueryClientProvider client={queryClient}>
        <Comic />
      </QueryClientProvider>
    </StrictMode>,
  )
}

init()
