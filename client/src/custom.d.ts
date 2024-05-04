import type { ComicGlobal } from '.'

declare global {
  interface Window {
    Comic: ComicGlobal
  }

  interface Document {
    webkitFullscreenElement?: Element
  }

  interface HTMLElement {
    requestFullscreen?: () => Promise<void>
    webkitRequestFullscreen?: () => Promise<void>
  }
}
