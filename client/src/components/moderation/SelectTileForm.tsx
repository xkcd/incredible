import React, { useCallback } from 'react'

export default function SelectTileForm({
  xt,
  yt,
  onGotoLoc,
  onNextEmpty,
}: {
  xt: number
  yt: number
  onGotoLoc: (xt: number, yt: number) => void
  onNextEmpty: () => void
}) {
  const handleChangeX = useCallback(
    (ev: React.ChangeEvent<HTMLInputElement>) => {
      onGotoLoc(Number(ev.target.value), yt)
    },
    [onGotoLoc, yt],
  )

  const handleChangeY = useCallback(
    (ev: React.ChangeEvent<HTMLInputElement>) => {
      onGotoLoc(xt, Number(ev.target.value))
    },
    [onGotoLoc, xt],
  )

  return (
    <div
      css={{ display: 'flex', flexDirection: 'row', flexWrap: 'wrap', gap: 8 }}
    >
      <form css={{ display: 'flex', gap: 8, input: { width: '5ch' } }}>
        <label htmlFor="gotoX">X:</label>
        <input
          type="number"
          value={xt}
          onChange={handleChangeX}
          id="gotoX"
        ></input>
        <label htmlFor="gotoY">Y:</label>
        <input
          type="number"
          value={yt}
          onChange={handleChangeY}
          id="gotoY"
        ></input>
      </form>
      <button onClick={onNextEmpty}>Go to random tile</button>
    </div>
  )
}
