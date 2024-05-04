import { CSSProperties, ReactNode } from 'react'
import comic from '../../comic.json'

export default function InnerComicBorder({
  style,
  children,
}: {
  style?: CSSProperties
  children: ReactNode
}) {
  return (
    <div
      css={{
        position: 'relative',
        width: comic.width,
        height: comic.height,
        '&:after': {
          content: '""',
          position: 'absolute',
          border: '2px solid black',
          inset: 0,
          zIndex: 100,
          pointerEvents: 'none',
        },
      }}
      style={style}
    >
      {children}
    </div>
  )
}
