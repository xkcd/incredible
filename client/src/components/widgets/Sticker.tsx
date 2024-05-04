import imgCat from '@art/cat_4x.png'
import imgDeterminism from '@art/determinism-sign_4x.png'
import imgFigure1Happy from '@art/figure1-happy_4x.png'
import imgFigure1Sad from '@art/figure1-sad_4x.png'
import imgFigure2Happy from '@art/figure2-happy_4x.png'
import imgFigure2Sad from '@art/figure2-sad_4x.png'
import imgFigure3Happy from '@art/figure3-happy_4x.png'
import imgFigure3Sad from '@art/figure3-sad_4x.png'
import imgKingbun from '@art/kingbun_4x.png'
import imgKnievel from '@art/knievel_4x.png'
import imgSquobject from '@art/squobject_4x.png'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { getPositionStyles } from '../positionStyles'

export const stickers = {
  'figure1-happy': imgFigure1Happy,
  'figure1-sad': imgFigure1Sad,
  'figure2-happy': imgFigure2Happy,
  'figure2-sad': imgFigure2Sad,
  'figure3-happy': imgFigure3Happy,
  'figure3-sad': imgFigure3Sad,
  knievel: imgKnievel,
  squobject: imgSquobject,
  determinism: imgDeterminism,
  kingbun: imgKingbun,
  cat: imgCat,
} as const

export type StickerName = keyof typeof stickers

const smolStickers = new Set<StickerName>(['cat', 'kingbun'])

export interface StickerWidget extends Vector, Angled {
  type: 'sticker'
  sticker: StickerName
}

export function StickerPreview({ sticker }: { sticker: StickerName }) {
  return (
    <ComicImage
      img={stickers[sticker]}
      css={{
        maxWidth: smolStickers.has(sticker) ? '40%' : '75%',
        maxHeight: '80%',
        width: 'auto',
        height: 'auto',
      }}
    />
  )
}

export function Sticker({
  id,
  onSelect,
  x,
  y,
  angle,
  sticker,
}: StickerWidget & EditableWidget) {
  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={stickers[sticker]}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
