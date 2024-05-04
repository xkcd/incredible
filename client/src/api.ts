import { QueryClient } from '@tanstack/react-query'
import createClient, { Middleware } from 'openapi-fetch'
import comic from '../comic.json'

import type { paths } from './generated/api-spec'

export const apiClient = createClient<paths>({
  baseUrl: process.env.API_ENDPOINT ?? comic.apiEndpoint,
})

const throwOnError: Middleware = {
  async onResponse(res) {
    if (res.status >= 400) {
      const body: string = await res.clone().text()
      throw new Error(body)
    }
    return undefined
  },
}

apiClient.use(throwOnError)

export const queryClient = new QueryClient()
