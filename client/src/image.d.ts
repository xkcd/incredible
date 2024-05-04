declare module '*.png' {
  interface ComicImage {
    width: number
    height: number
    url: {
      '4x': string
      '2x': string
    }
    srcSet: string
  }
  const content: ComicImage
  export default content
}
