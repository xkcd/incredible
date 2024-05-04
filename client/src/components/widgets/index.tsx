import assertNever from 'assert-never'
import { mapValues } from 'lodash'
import React, { FC } from 'react'
import { WidgetCollection } from '../../types'
import { useMachineTile } from '../MachineTileContext'
import { EditableWidget } from '../MachineTileEditor'
import { Anvil, AnvilPreview, AnvilWidget } from './Anvil'
import {
  Attractor,
  AttractorPreview,
  AttractorWidget,
  Repulsor,
  RepulsorPreview,
  RepulsorWidget,
} from './AttractorRepulsor'
import { BallStand, BallStandPreview, BallStandWidget } from './BallStand'
import { Board, BoardPreview, BoardWidget } from './Board'
import { Brick, BrickPreview, BrickWidget } from './Brick'
import { CatSwat, CatSwatPreview, CatSwatWidget } from './CatSwat'
import { Cup, CupPreview, CupWidget } from './Cup'
import { Cushion, CushionPreview, CushionWidget } from './Cushion'
import Fan, { FanPreview, FanWidget } from './Fan'
import { Hammer, HammerPreview, HammerWidget } from './Hammer'
import Hook, {
  FlippedHookPreview,
  HookPreview,
  HookWidget,
  LeftHook,
  LeftHookWidget,
} from './Hook'
import LeftBumper, { LeftBumperPreview, LeftBumperWidget } from './LeftBumper'
import Prism, { PrismPreview, PrismWidget } from './Prism'
import RightBumper, {
  RightBumperPreview,
  RightBumperWidget,
} from './RightBumper'
import RoundBumper, {
  RoundBumperPreview,
  RoundBumperWidget,
} from './RoundBumper'
import {
  Sticker,
  StickerName,
  StickerPreview,
  StickerWidget,
  stickers,
} from './Sticker'
import { Sword, SwordPreview, SwordWidget } from './Sword'
import SpokedWheel, { SpokedWheelPreview, SpokedWheelWidget } from './Wheel'

export type WidgetData =
  | BoardWidget
  | BrickWidget
  | HammerWidget
  | SwordWidget
  | AnvilWidget
  | FanWidget
  | AttractorWidget
  | RepulsorWidget
  | LeftBumperWidget
  | RightBumperWidget
  | HookWidget
  | LeftHookWidget
  | RoundBumperWidget
  | CushionWidget
  | PrismWidget
  //| QuantumGateSlowWidget
  //| QuantumGateFastWidget
  | SpokedWheelWidget
  | BallStandWidget
  | CupWidget
  | StickerWidget
  | CatSwatWidget
export type WidgetType = WidgetData['type']

export interface PaletteItem<T> {
  preview: FC
  create: (x: number, y: number) => T
  canRotate?: boolean
  canResize?: boolean
  isSquare?: boolean
}

export const widgetList: Partial<Record<WidgetType, PaletteItem<WidgetData>>> =
  {
    board: {
      preview: BoardPreview,
      create: (x: number, y: number) => ({
        type: 'board',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    hammer: {
      preview: HammerPreview,
      create: (x: number, y: number) => ({
        type: 'hammer',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    sword: {
      preview: SwordPreview,
      create: (x: number, y: number) => ({
        type: 'sword',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    hook: {
      preview: HookPreview,
      create: (x: number, y: number) => ({
        type: 'hook',
        x,
        y,
        angle: 0,
      }),
    },
    lefthook: {
      preview: FlippedHookPreview,
      create: (x: number, y: number) => ({
        type: 'lefthook',
        x,
        y,
        angle: 0,
      }),
    },
    anvil: {
      preview: AnvilPreview,
      create: (x: number, y: number) => ({
        type: 'anvil',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    brick: {
      preview: BrickPreview,
      create: (x: number, y: number) => ({
        type: 'brick',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    fan: {
      preview: FanPreview,
      create: (x: number, y: number) => ({
        type: 'fan',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    cushion: {
      preview: CushionPreview,
      create: (x: number, y: number) => ({
        type: 'cushion',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    roundbumper: {
      preview: RoundBumperPreview,
      create: (x: number, y: number) => ({
        type: 'roundbumper',
        x,
        y,
      }),
    },
    rightbumper: {
      preview: RightBumperPreview,
      create: (x: number, y: number) => ({
        type: 'rightbumper',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    leftbumper: {
      preview: LeftBumperPreview,
      create: (x: number, y: number) => ({
        type: 'leftbumper',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },

    attractor: {
      preview: AttractorPreview,
      create: (x: number, y: number) => ({
        type: 'attractor',
        x,
        y,
        width: 150,
        height: 10,
      }),
      canResize: true,
      isSquare: true,
    },
    repulsor: {
      preview: RepulsorPreview,
      create: (x: number, y: number) => ({
        type: 'repulsor',
        x,
        y,
        width: 150,
        height: 10,
      }),
      canResize: true,
      isSquare: true,
    } /*
  quantumgateslow: {
    preview: QuantumGateSlowPreview,
    create: (x: number, y: number) => ({
      type: 'quantumgateslow',
      x,
      y,
      width: 150,
      height: 10,
    }),
    canResize: true,
    isSquare: true,
  },
  quantumgatefast: {
    preview: QuantumGateFastPreview,
    create: (x: number, y: number) => ({
      type: 'quantumgatefast',
      x,
      y,
      width: 150,
      height: 10,
    }),
    canResize: true,
    isSquare: true,
  },
  */,
    prism: {
      preview: PrismPreview,
      create: (x: number, y: number) => ({
        type: 'prism',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    spokedwheel: {
      preview: SpokedWheelPreview,
      create: (x: number, y: number) => ({
        type: 'spokedwheel',
        x,
        y,
        speed: 25,
        angle: 0,
      }),
    },
    ballstand: {
      preview: BallStandPreview,
      create: (x: number, y: number) => ({
        type: 'ballstand',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    cup: {
      preview: CupPreview,
      create: (x: number, y: number) => ({
        type: 'cup',
        x,
        y,
        angle: 0,
      }),
      canRotate: true,
    },
    catswat: {
      preview: CatSwatPreview,
      create: (x: number, y: number) => ({
        type: 'catswat',
        x,
        y,
        angle: 0,
        catMass: 2,
      }),
      canRotate: true,
    },
  }

export const stickerList: Record<
  string,
  PaletteItem<StickerWidget>
> = mapValues(stickers, (_, sticker) => ({
  preview: () => <StickerPreview sticker={sticker as StickerName} />,
  create: (x: number, y: number) => ({
    type: 'sticker' as const,
    sticker: sticker as StickerName,
    x,
    y,
    angle: 0,
  }),
  canRotate: true,
}))

export function Widget(
  props: WidgetData & Partial<EditableWidget> & { id: EditableWidget['id'] },
) {
  const { type } = props

  if (type === 'board') {
    return <Board {...props} />
  } else if (type === 'brick') {
    return <Brick {...props} />
  } else if (type === 'hammer') {
    return <Hammer {...props} />
  } else if (type === 'sword') {
    return <Sword {...props} />
  } else if (type === 'anvil') {
    return <Anvil {...props} />
  } else if (type === 'fan') {
    return <Fan {...props} />
  } else if (type === 'leftbumper') {
    return <LeftBumper {...props} />
  } else if (type === 'rightbumper') {
    return <RightBumper {...props} />
  } else if (type === 'hook') {
    return <Hook {...props} />
  } else if (type === 'lefthook') {
    return <LeftHook {...props} />
  } else if (type === 'roundbumper') {
    return <RoundBumper {...props} />
  } else if (type === 'cushion') {
    return <Cushion {...props} />
  } else if (type === 'prism') {
    return <Prism {...props} />
  } else if (type === 'spokedwheel') {
    return <SpokedWheel {...props} />
  } else if (type === 'attractor') {
    return <Attractor {...props} />
  } else if (type === 'repulsor') {
    return <Repulsor {...props} />
  } else if (type === 'ballstand') {
    return <BallStand {...props} />
  } else if (type === 'cup') {
    return <Cup {...props} />
  } else if (type === 'catswat') {
    return <CatSwat {...props} />
  } else if (type === 'sticker') {
    return <Sticker {...props} /> /*
    /*
  } else if (type === 'quantumgateslow') {
    return <QuantumGateSlow {...props} />
  } else if (type === 'quantumgatefast') {
    return <QuantumGateFast {...props} />
  */
  } else {
    assertNever(type, true)
  }
}

function WidgetsUnmemoized({
  widgets,
  onSelect,
  selectedId,
}: {
  widgets: WidgetCollection
  onSelect?: EditableWidget['onSelect']
  selectedId?: string | undefined
}) {
  const {
    bounds: [offsetX, offsetY],
  } = useMachineTile()

  return Object.entries(widgets).map(([id, widget]) => {
    let { x, y } = widget
    x = x + offsetX
    y = y + offsetY

    return (
      <Widget
        key={id}
        {...widget}
        id={id}
        x={x}
        y={y}
        onSelect={onSelect}
        isSelected={selectedId === id}
      />
    )
  })
}

// Separated to work around an eslint react/prop-types false positive.
export const Widgets = React.memo(WidgetsUnmemoized)
