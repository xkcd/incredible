import { QueryClientProvider } from '@tanstack/react-query'
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { queryClient } from '../api'
import Moderator from '../components/moderation/Moderator'

const root = createRoot(document.getElementsByTagName('main')[0])
root.render(
  <StrictMode>
    <QueryClientProvider client={queryClient}>
      <Moderator />
    </QueryClientProvider>
  </StrictMode>,
)
