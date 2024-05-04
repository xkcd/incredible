import {
  queryOptions,
  useMutation,
  useQueries,
  useQuery,
} from '@tanstack/react-query'
import { apiClient, queryClient } from '../../api'
import { MachineSnapshot } from '../../lib/snapshot'
import { blueprintQueryOptions } from '../useMetaMachineClient'
import { CandidateMap } from './modTypes'

export function puzzleQueryOptions(puzzleId: string | undefined) {
  return queryOptions({
    enabled: puzzleId != null,
    queryKey: ['moderator', 'puzzle', puzzleId],
    queryFn: async ({ signal }) => {
      const { data } = await apiClient.GET('/moderate/puzzle/{puzzleid}', {
        params: {
          path: {
            puzzleid: puzzleId!,
          },
        },
        credentials: 'include',
        signal,
      })
      return data
    },
    staleTime: Infinity,
  })
}

export function useModeratorMachine() {
  return useQuery({
    queryKey: ['moderator', 'machine'],
    queryFn: async ({ signal }) => {
      const { data } = await apiClient.GET('/moderate/machine/current', {
        credentials: 'include',
        signal,
      })
      return data
    },
    staleTime: 30 * 1000,
  })
}

export function useCandidateBlueprints({ puzzleId }: { puzzleId: string }) {
  return useQuery<CandidateMap>({
    queryKey: ['moderator', 'blueprints', puzzleId],
    queryFn: async ({ signal }) => {
      const { data } = await apiClient.GET(
        '/moderate/puzzle/{puzzleid}/blueprint',
        {
          params: {
            path: {
              puzzleid: puzzleId,
            },
          },
          credentials: 'include',
          signal,
        },
      )
      return new Map(data)
    },
  })
}

export function useBlueprint(blueprintId: string | undefined) {
  return useQuery(blueprintQueryOptions(blueprintId))
}

export function useContextBlueprints(blueprintIds: Array<string | undefined>) {
  return useQueries({
    queries: blueprintIds.map((id) => blueprintQueryOptions(id)),
  })
}

export function useContextPuzzles(puzzleIds: Array<string | undefined>) {
  return useQueries({
    queries: puzzleIds.map(puzzleQueryOptions),
  })
}

export function useApproveBlueprint() {
  return useMutation({
    mutationFn: ({
      xt,
      yt,
      blueprintId,
      snapshot,
    }: {
      xt: number
      yt: number
      blueprintId: string
      snapshot: MachineSnapshot
    }) => {
      return apiClient.POST('/moderate/build/{X}/{Y}', {
        params: {
          path: {
            X: xt,
            Y: yt,
          },
        },
        body: {
          blueprint: blueprintId,
          snapshot: snapshot as unknown as Record<string, unknown>,
        },
        credentials: 'include',
      })
    },
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['moderator', 'machine'] })
    },
  })
}

export function useBurnBlueprint() {
  return useMutation({
    mutationFn: ({
      blueprintId,
    }: {
      puzzleId: string
      blueprintId: string
    }) => {
      return apiClient.POST('/moderate/burn/{blueprintid}', {
        params: {
          path: {
            blueprintid: blueprintId,
          },
        },
        credentials: 'include',
      })
    },
    onSuccess: (_data, { puzzleId }) => {
      void queryClient.invalidateQueries({
        queryKey: ['moderator', 'blueprints', puzzleId],
      })
    },
  })
}

export function useReissuePuzzle() {
  return useMutation({
    mutationFn: ({ puzzleId }: { puzzleId: string }) => {
      return apiClient.POST('/moderate/puzzle/{puzzleid}/reissue', {
        params: {
          path: {
            puzzleid: puzzleId,
          },
        },
        credentials: 'include',
      })
    },
  })
}
