import { components } from '../../generated/api-spec'

export type ModLocation = {
  puzzle: string
  blueprint?: string
  xt: number
  yt: number
}

export type ServerBlueprint = components['schemas']['Blueprint']

export type CandidateMap = Map<string, ServerBlueprint>

export type ModMachine = components['schemas']['VersionedMachine ModData']
