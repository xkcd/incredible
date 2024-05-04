import {
  queryOptions,
  useMutation,
  useQueries,
  useQuery,
} from '@tanstack/react-query'
import { dropRightWhile, isNull } from 'lodash'
import { useCallback, useMemo } from 'react'
import invariant from 'tiny-invariant'
import { apiClient, queryClient } from '../api'
import { MachineSnapshot } from '../lib/snapshot'
import {
  gridDimensions,
  gridViewBounds,
  iterTiles,
  tileKey,
} from '../lib/tiles'
import { Bounds, Puzzle, PuzzleOrder, WidgetCollection } from '../types'
import { WidgetData } from './widgets'

export interface TileData {
  blueprintId: string
  title: string
  puzzle: Puzzle
  widgets: WidgetCollection
  snapshot?: MachineSnapshot
}

export type GetMachineFunction = (
  xt: number,
  yt: number,
) => TileData | undefined

export interface MetaMachineInfo {
  version: number
  tileWidth: number
  tileHeight: number
  tilesX: number
  tilesY: number
  getMachine: GetMachineFunction
  hasBlueprint: (xt: number, yt: number) => boolean
  msPerBall: number
}

export interface MetaMachineClient {
  isLoading: boolean
  allTilesLoaded: boolean
  metaMachine: MetaMachineInfo | null
}

export interface SavedBlueprintLocation {
  blueprintId: string
  xt: number
  yt: number
}

export type SavedMachine = SavedBlueprintLocation & TileData

export function blueprintQueryOptions(blueprintid: string | undefined) {
  return queryOptions({
    enabled: blueprintid != null,
    queryKey: ['folio', blueprintid],
    queryFn: async ({ signal }) => {
      invariant(blueprintid, 'blueprintid must be non-null')
      const { data } = await apiClient.GET('/folio/{blueprintid}', {
        params: {
          path: {
            blueprintid,
          },
        },
        signal,
      })
      return data
    },
    staleTime: Infinity,
  })
}

export function useMetaMachineClient({
  viewBounds,
  lastSubmission,
  version: fetchVersion,
  fetchOutset = 2000,
}: {
  viewBounds: Bounds
  lastSubmission?: SavedMachine | undefined
  version?: number
  fetchOutset?: number
}): MetaMachineClient {
  const { data: machineData, isLoading } = useQuery({
    queryKey: ['machine', fetchVersion ?? 'current'],
    queryFn: async ({ signal }) => {
      if (fetchVersion != null) {
        const { data } = await apiClient.GET('/machine/{version}', {
          params: { path: { version: fetchVersion } },
          signal,
        })
        return data
      } else {
        const { data } = await apiClient.GET('/machine/current', { signal })
        return data
      }
    },
  })

  const tileWidth = machineData?.tile_size.x ?? 0
  const tileHeight = machineData?.tile_size.y ?? 0
  const msPerBall = machineData?.ms_per_ball ?? Infinity
  const version = machineData?.version ?? 0

  const trimmedGrid = machineData?.grid
    ? dropRightWhile(machineData.grid, (row, idx) => {
        if (idx === 0) {
          return false
        }
        if (lastSubmission && idx <= lastSubmission.yt) {
          return false
        }
        return row.every(isNull)
      })
    : []

  const [tilesX, tilesY] = machineData ? gridDimensions(trimmedGrid) : [0, 0]

  const tileBounds: Bounds = machineData
    ? gridViewBounds(
        viewBounds,
        tilesX,
        tilesY,
        tileWidth,
        tileHeight,
        fetchOutset,
      )
    : [0, 0, 0, 0]

  const tiles = [...iterTiles(...tileBounds)]
  const { sparseTileData, allTilesLoaded } = useQueries({
    queries: machineData
      ? tiles.map(([xt, yt]) => blueprintQueryOptions(trimmedGrid[yt][xt]))
      : [],
    combine: (results) => {
      const sparseTileData: Record<string, TileData> = {}
      let allTilesLoaded = results.length > 0
      for (let i = 0; i < results.length; i++) {
        allTilesLoaded &&= !results[i].isLoading

        const data = results[i].data
        const [xt, yt] = tiles[i]
        if (!data) {
          continue
        }

        sparseTileData[tileKey(xt, yt)] = {
          blueprintId: trimmedGrid[yt][xt],
          title: data.blueprint.title,
          puzzle: data.puzzle,
          widgets: data.blueprint.widgets as Record<string, WidgetData>,
          snapshot: data.snapshot as unknown as MachineSnapshot,
        }
      }
      return { sparseTileData, allTilesLoaded }
    },
  })

  const getMachine = useCallback(
    (xt: number, yt: number) =>
      xt === lastSubmission?.xt && yt === lastSubmission?.yt
        ? lastSubmission
        : sparseTileData[tileKey(xt, yt)],
    [lastSubmission, sparseTileData],
  )

  const hasBlueprint = useCallback(
    (xt: number, yt: number) => machineData?.grid[yt][xt] != null,
    [machineData?.grid],
  )

  return useMemo(
    () => ({
      isLoading,
      allTilesLoaded,
      metaMachine: machineData
        ? {
            version,
            tilesX,
            tilesY,
            tileWidth,
            tileHeight,
            getMachine,
            hasBlueprint,
            msPerBall,
          }
        : null,
    }),
    [
      allTilesLoaded,
      getMachine,
      hasBlueprint,
      isLoading,
      machineData,
      msPerBall,
      tileHeight,
      tileWidth,
      tilesX,
      tilesY,
      version,
    ],
  )
}

export function useGetPuzzles(): PuzzleOrder[] | undefined {
  const { data, isStale } = useQuery({
    queryKey: ['puzzle'],
    queryFn: async ({ signal }) => {
      const { data, response } = await apiClient.GET('/puzzle', {
        signal,
      })

      const workOrder = response.headers.get('X-WorkOrder')
      if (!workOrder) {
        return
      }

      const puzzles = data
        ? Object.entries(data).map(([puzzleId, puzzleData]) => ({
            id: puzzleId,
            workOrder: workOrder,
            inputs: puzzleData.inputs,
            outputs: puzzleData.outputs,
          }))
        : undefined

      return puzzles
    },
    staleTime: Infinity,
  })

  return !isStale ? data : undefined
}

export function useSubmitBlueprint() {
  return useMutation({
    mutationFn: async ({
      puzzleId,
      workOrder,
      title,
      widgets,
    }: {
      puzzleId: string
      workOrder: string
      title: string
      widgets: WidgetCollection
    }) => {
      const { data } = await apiClient.POST('/blueprint/file', {
        body: { puzzle: puzzleId, title, widgets },
        headers: { 'X-WorkOrder': workOrder },
      })

      if (!data) {
        return null
      }

      const [blueprintId, [xt, yt]] = data
      return { blueprintId, xt, yt }
    },
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['puzzle'] })
    },
  })
}
