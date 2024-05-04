import { ComicImage } from './ComicImage'

import ballBlueImg from '@art/ball-blue_4x.png'
import ballGreenImg from '@art/ball-green_4x.png'
import ballRedImg from '@art/ball-red_4x.png'
import ballYellowImg from '@art/ball-yellow_4x.png'
import { css, keyframes } from '@emotion/react'

const rotate = keyframes`
  from {
    transform: rotate(0deg);
  }
  to {
    transform: rotate(360deg);
  }
`

const fadeIn = keyframes`
  0% {
    opacity: 0;
  }
  50% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
`

const counterRotateStyle = css({
  animation: `${rotate} 1s linear infinite reverse`,
})

export default function LoadingSpinner({ className }: { className?: string }) {
  return (
    <div
      css={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        height: '100%',
      }}
      className={className}
    >
      <div
        css={{
          position: 'absolute',
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 4,
          margin: 4,
          animation: `1s linear infinite ${rotate}, 500ms ease-in ${fadeIn}`,
        }}
      >
        <ComicImage css={counterRotateStyle} img={ballBlueImg} />
        <ComicImage css={counterRotateStyle} img={ballGreenImg} />
        <ComicImage css={counterRotateStyle} img={ballRedImg} />
        <ComicImage css={counterRotateStyle} img={ballYellowImg} />
      </div>
    </div>
  )
}
