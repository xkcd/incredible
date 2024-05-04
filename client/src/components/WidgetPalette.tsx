import imgEmergencyStop from '@art/emergency-stop_4x.png'
import imgTrash from '@art/trash_4x.png'
import { PropsOf, css } from '@emotion/react'
import { motion } from 'framer-motion'
import { ComicImage } from './ComicImage'
import { dialogStyles } from './SwooshyDialog'
import { MAX_WIDGET_COUNT } from './constants'
import { PaletteItem, WidgetData, stickerList, widgetList } from './widgets'

export const comicDropShadow = css({
  filter: 'drop-shadow(2px 2px 0 rgba(0, 0, 0, 0.5))',
})

export function ToolButton({
  disabled,
  className,
  'aria-label': ariaLabel,
  onClick,
  children,
  ...props
}: PropsOf<typeof motion.button>) {
  return (
    <motion.button
      {...props}
      onClick={onClick}
      disabled={disabled}
      whileTap={disabled ? undefined : { scale: 0.9 }}
      css={{
        padding: 0,
        border: 'none',
        background: 'none',
        cursor: disabled ? 'default' : 'pointer',
        zIndex: 20,
      }}
      className={className}
      aria-label={ariaLabel}
    >
      {children}
    </motion.button>
  )
}

export default function WidgetPalette({
  className,
  widgetCount,
  isHorizontal,
  onAdd,
  onTrash,
  onEmergencyStop,
}: {
  className?: string
  widgetCount: number
  isHorizontal: boolean
  onAdd: (create: PaletteItem<WidgetData>['create']) => void
  onTrash: () => void
  onEmergencyStop: () => void
}) {
  const canAddWidgets = widgetCount < MAX_WIDGET_COUNT

  function renderList(list: Record<string, PaletteItem<WidgetData>>) {
    return Object.entries(list).map(
      ([type, { preview: WidgetPreview, create }]) => (
        <div
          key={type}
          onClick={() => onAdd(create)}
          css={{
            flexShrink: 0,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            maxWidth: '100%',
            maxHeight: '100%',
            aspectRatio: 1,
            cursor: 'pointer',
            '@media (pointer: fine)': {
              ':hover': {
                background: '#ccc',
              },
            },
          }}
        >
          <WidgetPreview />
        </div>
      ),
    )
  }

  return (
    <div
      css={[
        dialogStyles,
        {
          display: 'flex',
          alignItems: 'center',
          gap: 12,
          zIndex: 70,
        },
      ]}
      style={
        isHorizontal
          ? {
              flexDirection: 'row',
              paddingRight: 12,
            }
          : {
              flexDirection: 'column',
              paddingBottom: 12,
            }
      }
      className={className}
    >
      <div
        css={[
          {
            flex: 1,
            display: 'flex',
            flexDirection: isHorizontal ? 'row' : 'column',
            maxWidth: '100%',
            maxHeight: '100%',
            overflow: 'auto',
            scrollbarWidth: 'thin',
            opacity: canAddWidgets ? 1 : 0.5,
            pointerEvents: canAddWidgets ? 'all' : 'none',
          },
          isHorizontal && { height: '100%' },
        ]}
      >
        {renderList(widgetList)}
        <div
          css={{
            borderBottom: '1px dashed black',
            paddingTop: 10,
            marginBottom: 10,
            marginLeft: 4,
            marginRight: 4,
          }}
        />
        {renderList(stickerList)}
      </div>
      {widgetCount > 0.75 * MAX_WIDGET_COUNT && (
        <div
          css={{
            display: 'flex',
            height: 26,
            alignItems: 'center',
            justifyContent: 'center',
            borderTop: '2px solid black',
            color: canAddWidgets ? 'black' : 'red',
          }}
        >
          {widgetCount} / {MAX_WIDGET_COUNT}
        </div>
      )}
      <ToolButton
        onClick={onTrash}
        aria-label="Delete selection"
        css={{
          height: imgTrash.height,
        }}
      >
        <ComicImage img={imgTrash} />
      </ToolButton>
      <ToolButton
        onClick={onEmergencyStop}
        aria-label="Emergency stop"
        css={{
          height: imgEmergencyStop.height,
        }}
      >
        <ComicImage img={imgEmergencyStop} />
      </ToolButton>
    </div>
  )
}
