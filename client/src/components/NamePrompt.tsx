import { css } from '@emotion/react'
import { sample } from 'lodash'
import React, { useCallback, useMemo, useState } from 'react'
import { SwooshyDialog, dialogStyles } from './SwooshyDialog'

const superlatives = [
  'illustrious',
  'magnificent',
  'peculiar',
  'fascinating',
  'marvelous',
  'confounding',
]

const nouns = ['invention', 'device', 'machine', 'contraption']

const namePromptStyles = css({
  fontFamily: 'xkcd-Regular-v3',
  padding: 16,
  userSelect: 'none',
  display: 'flex',
  flexDirection: 'column',
  gap: 16,
  width: 350,

  '.title': {
    fontSize: '20px',
    lineHeight: '135%',
  },

  'button, input': {
    fontFamily: 'xkcd-Regular-v3',
    border: '2px solid black',
    padding: 8,
    borderRadius: 4,
  },

  input: {
    fontSize: '20px',
    background: '#eee',
  },

  button: {
    fontSize: '16px',
    background: 'white',
    boxShadow: '3px 3px 0 rgba(0, 0, 0, 0.5)',
    cursor: 'pointer',

    '&:active': {
      transform: 'translate(3px, 3px)',
      background: '#eee',
      boxShadow: 'none',
    },

    '&:disabled': {
      pointerEvents: 'none',
    },
  },
})

export function NamePrompt({
  onSubmit,
  onCancel,
}: {
  onSubmit: (name: string) => void
  onCancel: () => void
}) {
  const description = useMemo(
    () => `${sample(superlatives)} ${sample(nouns)}`,
    [],
  )

  const [name, setName] = useState('')

  const handleNameChange = useCallback(
    (ev: React.ChangeEvent<HTMLInputElement>) => {
      setName(ev.target.value)
    },
    [],
  )

  const handleSubmit = useCallback(
    (ev: React.FormEvent<HTMLFormElement>) => {
      ev.preventDefault()
      onSubmit(name)
    },
    [name, onSubmit],
  )

  return (
    <SwooshyDialog onDismiss={onCancel}>
      <form
        className="wrapper"
        css={[dialogStyles, namePromptStyles]}
        onSubmit={handleSubmit}
      >
        <span className="title">
          What will you name this
          <br />
          {description}?
        </span>
        <input
          css={{
            textAlign: 'center',
            fontSize: '20px',
          }}
          value={name}
          onChange={handleNameChange}
          autoFocus
        />
        <div
          css={{
            display: 'flex',
            justifyContent: 'center',
            gap: 12,
          }}
        >
          <button onClick={onCancel} type="button">
            Cancel
          </button>
          <button disabled={name.length === 0} type="submit">
            Submit
          </button>
        </div>
      </form>
    </SwooshyDialog>
  )
}
