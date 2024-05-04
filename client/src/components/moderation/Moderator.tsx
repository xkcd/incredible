import { Global } from '@emotion/react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { clamp } from 'lodash'
import { useCallback, useEffect, useMemo, useState } from 'react'
import { components } from '../../generated/api-spec'
import { gridDimensions } from '../../lib/tiles'
import LoadingSpinner from '../LoadingSpinner'
import { paramToNumber, useLocationHashParams } from '../useLocationHashParams'
import { BlueprintButton } from './BlueprintButton'
import ContextGridForMachineAt from './ContextGridForMachineAt'
import { LiveMachinePreview } from './LiveMachinePreview'
import SelectTileForm from './SelectTileForm'
import { ModLocation } from './modTypes'
import { getEmptyTile, locFromPosition, sortCandidateMap } from './modUtils'
import {
  puzzleQueryOptions,
  useBlueprint,
  useCandidateBlueprints,
  useModeratorMachine,
} from './moderatorClient'

function useModHashParams(): {
  hashLoc: ModLocation | null
  setHashLoc: (loc: ModLocation) => void
} {
  const { locationHashParams, setLocationHashParams } = useLocationHashParams()

  const hashLoc = useMemo(() => {
    const blueprint = locationHashParams.get('blueprint') ?? undefined
    const puzzle = locationHashParams.get('puzzle')
    const xt = paramToNumber(locationHashParams.get('xt'))
    const yt = paramToNumber(locationHashParams.get('yt'))
    if (!puzzle || xt == null || yt == null) {
      return null
    }
    return {
      blueprint,
      puzzle,
      xt,
      yt,
    }
  }, [locationHashParams])

  const setHashLoc = useCallback(
    (loc: ModLocation) => {
      setLocationHashParams({
        blueprint: loc.blueprint,
        puzzle: loc.puzzle,
        xt: String(loc.xt),
        yt: String(loc.yt),
      })
    },
    [setLocationHashParams],
  )

  return { hashLoc, setHashLoc }
}

function BlueprintModerator({
  modMachine,
}: {
  modMachine: components['schemas']['VersionedMachine ModData']
}) {
  const { hashLoc, setHashLoc } = useModHashParams()

  const [loc, setLoc] = useState(() =>
    hashLoc
      ? locFromPosition(modMachine.grid, hashLoc.xt, hashLoc.yt)
      : getEmptyTile(modMachine),
  )

  const queryClient = useQueryClient()

  const { data: locFolio } = useBlueprint(loc.blueprint)
  const { data: candidateBlueprints } = useCandidateBlueprints({
    puzzleId: loc.puzzle,
  })

  const [selectedBlueprintId, setSelectedBlueprintId] = useState<
    string | undefined
  >(hashLoc?.blueprint)

  useEffect(() => {
    setHashLoc({
      ...loc,
      blueprint: selectedBlueprintId,
    })
  }, [loc, selectedBlueprintId, setHashLoc])

  useEffect(() => {
    if (!hashLoc) {
      return
    }
    setLoc(locFromPosition(modMachine.grid, hashLoc.xt, hashLoc.yt))
    setSelectedBlueprintId(hashLoc.blueprint)
  }, [hashLoc, modMachine.grid])

  const sortedCandidateBlueprints = useMemo(
    () =>
      candidateBlueprints
        ? sortCandidateMap(candidateBlueprints, loc.blueprint)
        : null,
    [candidateBlueprints, loc.blueprint],
  )

  const selectedBlueprint =
    selectedBlueprintId === loc.blueprint
      ? locFolio?.blueprint
      : selectedBlueprintId
        ? candidateBlueprints?.get(selectedBlueprintId)
        : undefined

  const { data: locPuzzle } = useQuery(puzzleQueryOptions(loc.puzzle))

  const handleGotoLoc = useCallback(
    (rawXt: number, rawYt: number) => {
      const [tilesX, tilesY] = gridDimensions(modMachine.grid)
      const xt = clamp(rawXt, 0, tilesX - 1)
      const yt = clamp(rawYt, 0, tilesY - 1)
      setLoc({
        xt,
        yt,
        ...modMachine.grid[yt][xt],
      })
      void queryClient.invalidateQueries({
        queryKey: ['moderator', 'machine'],
      })
    },
    [modMachine.grid, queryClient],
  )

  const handleNextEmpty = useCallback(() => {
    void queryClient
      .invalidateQueries({
        queryKey: ['moderator', 'machine'],
      })
      .then(() => {
        setLoc(getEmptyTile(modMachine))
      })
  }, [modMachine, queryClient])

  const handleNextBlueprint = useCallback(() => {
    if (!sortedCandidateBlueprints || !selectedBlueprintId) {
      return
    }
    const currentIdx = sortedCandidateBlueprints.findIndex(
      ([id]) => id === selectedBlueprintId,
    )
    if (currentIdx === -1) {
      return
    }
    const nextItem = sortedCandidateBlueprints[currentIdx + 1]
    if (nextItem) {
      setSelectedBlueprintId(nextItem[0])
    }
  }, [selectedBlueprintId, sortedCandidateBlueprints])

  // If the selected blueprint id isn't valid, pick a suitable one
  useEffect(() => {
    if (
      selectedBlueprintId != null &&
      (selectedBlueprintId === loc.blueprint ||
        !candidateBlueprints ||
        candidateBlueprints.has(selectedBlueprintId))
    ) {
      return
    }

    if (loc.blueprint != null) {
      setSelectedBlueprintId(loc.blueprint)
      return
    }

    if (candidateBlueprints) {
      const firstId = candidateBlueprints.keys().next()
      setSelectedBlueprintId(firstId.done === false ? firstId.value : undefined)
    }
  }, [selectedBlueprintId, candidateBlueprints, loc.blueprint])

  return (
    <div>
      <Global
        styles={{
          body: {
            fontFamily: 'xkcd-Regular-v3',
          },
          'h1, h2, h3': {
            fontWeight: 'normal',
          },
        }}
      />
      <h1>Incredible Modview</h1>
      <div
        css={{
          display: 'flex',
          alignItems: 'top',
          justifyContent: 'center',
          minHeight: 700,

          '& > div': {
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            flex: 1,
            gap: 16,
          },

          h2: {
            margin: 0,
          },
        }}
      >
        <div>
          <h2>Context Window</h2>
          <ContextGridForMachineAt
            modMachine={modMachine}
            selectedBlueprint={selectedBlueprint}
            onSelectLocation={handleGotoLoc}
            {...loc}
          />
          <SelectTileForm
            xt={loc.xt}
            yt={loc.yt}
            onGotoLoc={handleGotoLoc}
            onNextEmpty={handleNextEmpty}
          />
        </div>
        <div>
          <h2>Candidate Machine</h2>
          {locPuzzle ? (
            <LiveMachinePreview
              key={selectedBlueprintId}
              loc={loc}
              modMachine={modMachine}
              puzzle={locPuzzle}
              blueprintId={selectedBlueprintId}
              blueprint={selectedBlueprint}
              onNextBlueprint={handleNextBlueprint}
            />
          ) : (
            <LoadingSpinner />
          )}
        </div>
      </div>
      <h2 css={{ marginTop: 48 }}>Candidate Machine Options</h2>
      {
        <div
          css={{
            display: 'flex',
            flexDirection: 'row',
            flexWrap: 'wrap',
            alignItems: 'center',
            justifyContent: 'center',
            margin: 16,
            gap: 8,
          }}
        >
          {loc.blueprint && locFolio && (
            <BlueprintButton
              key={loc.blueprint}
              blueprintId={loc.blueprint}
              blueprint={locFolio.blueprint}
              puzzle={locFolio.puzzle}
              tileWidth={modMachine.tile_size.x}
              tileHeight={modMachine.tile_size.y}
              isSelected={loc.blueprint === selectedBlueprintId}
              isApproved={true}
              onSelect={setSelectedBlueprintId}
            />
          )}
          {sortedCandidateBlueprints && locPuzzle
            ? sortedCandidateBlueprints.map(([blueprintId, blueprint]) => (
                <BlueprintButton
                  key={blueprintId}
                  blueprintId={blueprintId}
                  blueprint={blueprint}
                  puzzle={locPuzzle}
                  tileWidth={modMachine.tile_size.x}
                  tileHeight={modMachine.tile_size.y}
                  isSelected={blueprintId === selectedBlueprintId}
                  isApproved={blueprintId === loc.blueprint}
                  onSelect={setSelectedBlueprintId}
                />
              ))
            : null}
        </div>
      }
    </div>
  )
}

export default function Moderator() {
  const { data: modMachine } = useModeratorMachine()

  if (!modMachine) {
    return <LoadingSpinner css={{ height: '100vh' }} />
  }

  return <BlueprintModerator modMachine={modMachine} />
}
