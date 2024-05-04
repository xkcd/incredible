import { useCallback, useEffect, useMemo, useState } from 'react'

export function paramToNumber(text: string | null | undefined) {
  if (text == null) {
    return undefined
  }

  const val = Number(text)
  if (isNaN(val)) {
    return undefined
  }

  return val
}

export function useLocationHashParams() {
  const [, triggerUpdate] = useState(0)

  useEffect(() => {
    function handleHashChange() {
      triggerUpdate((x) => x + 1)
    }
    window.addEventListener('hashchange', handleHashChange)
    return () => {
      window.removeEventListener('hashchange', handleHashChange)
    }
  }, [])

  const params = useMemo(
    () => new URLSearchParams(location.hash.slice(1)),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [location.hash],
  )

  const setLocationHashParams = useCallback(
    (data: Record<string, string | null | undefined>) => {
      const params = new URLSearchParams()
      for (const [k, v] of Object.entries(data)) {
        if (v == null) {
          continue
        }
        params.set(k, v)
      }

      const newHash = new URLSearchParams(params).toString()
      const currentURLWithoutHash = window.location.href.split('#')[0]
      window.location.replace(`${currentURLWithoutHash}#${newHash}`)
    },
    [],
  )

  return {
    locationHashParams: params,
    setLocationHashParams,
  }
}
