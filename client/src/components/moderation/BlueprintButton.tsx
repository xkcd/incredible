import useIntersectionObserver from '@react-hook/intersection-observer'
import { truncate } from 'lodash'
import { useCallback, useRef } from 'react'
import { Puzzle } from '../../types'
import { default as ModMachineTileView } from './ModMachineTileView'
import { ServerBlueprint } from './modTypes'

export function BlueprintButton({
  blueprintId,
  blueprint,
  puzzle,
  tileWidth,
  tileHeight,
  isSelected,
  isApproved,
  onSelect,
}: {
  blueprintId: string
  blueprint: ServerBlueprint
  puzzle: Puzzle
  tileWidth: number
  tileHeight: number
  isSelected: boolean
  isApproved: boolean
  onSelect: (blueprintId: string) => void
}) {
  const ref = useRef<HTMLButtonElement>(null)
  const { isIntersecting } = useIntersectionObserver(ref)

  const handleClick = useCallback(() => {
    onSelect(blueprintId)
  }, [blueprintId, onSelect])

  return (
    <button
      ref={ref}
      css={{
        position: 'relative',
        background: 'none',
        border: 'none',
        outline: `3px solid ${isSelected ? 'blue' : isApproved ? 'green' : 'black'}`,
        borderRadius: 4,
        width: 150,
        height: 150,
        padding: 0,
      }}
      onClick={handleClick}
    >
      <div
        css={{
          position: 'absolute',
          right: 2,
          bottom: 2,
          color: isApproved ? 'green' : isSelected ? 'blue' : 'gray',
          fontWeight: 'bold',
          background: 'white',
          textAlign: 'right',
          zIndex: 10,
        }}
      >
        {isApproved
          ? 'current approved'
          : isSelected
            ? 'viewing'
            : truncate(blueprint.title, { length: 42 })}
      </div>

      {isIntersecting && (
        <ModMachineTileView
          puzzle={puzzle}
          blueprint={blueprint}
          width={150}
          height={150}
          tileWidth={tileWidth}
          tileHeight={tileHeight}
        />
      )}
    </button>
  )
}
