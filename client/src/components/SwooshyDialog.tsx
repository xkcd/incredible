import { css } from '@emotion/react'
import { motion } from 'framer-motion'
import React, { ReactNode, useCallback, useRef } from 'react'

export const dialogStyles = css({
  border: '2px solid black',
  background: 'white',
  boxShadow: '5px 5px 0 rgba(0, 0, 0, 0.5)',
  borderRadius: 4,
})

export function SwooshyDialog({
  className,
  onDismiss,
  children,
}: {
  className?: string
  onDismiss?: () => void
  children: ReactNode
}) {
  const ref = useRef<HTMLDivElement>(null)
  const handleClick = useCallback(
    (ev: React.MouseEvent<HTMLDivElement>) => {
      if (ev.target === ev.currentTarget) {
        onDismiss?.()
      }
    },
    [onDismiss],
  )

  return (
    <motion.div
      ref={ref}
      onClick={handleClick}
      initial={{ scale: 0, opacity: 0 }}
      animate={{ scale: 1, opacity: 1 }}
      exit={{ scale: 0, opacity: 0 }}
      transition={{ type: 'spring', duration: 0.65 }}
      css={{
        position: 'absolute',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        width: '100%',
        height: '100%',
        zIndex: 50,
        '.wrapper': {
          display: 'flex',
          position: 'relative',
        },
      }}
      className={className}
    >
      {children}
    </motion.div>
  )
}
