import loaderUtils from 'loader-utils'
import sharp from 'sharp'
import { callbackify } from 'util'

async function processImage(inputBuffer) {
  const options = this.getOptions()

  const img = sharp(inputBuffer)
  const meta = await img.metadata()

  const scaleImage = async (scale) => {
    const { data, info } = await img
      .resize({
        width: Math.floor(meta.width * (scale / options.baseScale)),
      })
      .png({ palette: !!options.quant })
      .toBuffer({ resolveWithObject: true })

    const interpolatedName = loaderUtils.interpolateName(this, options.name, {
      content: data,
    })
    this.emitFile(interpolatedName, data)
    return { scale, data, info, interpolatedName }
  }

  const publicPath = options.publicPath ? options.publicPath : ''

  const scaleResults = await Promise.all(options.scales.map(scaleImage))
  const url = {}
  const srcSetItems = []
  for (const result of scaleResults) {
    const imgURL = publicPath + result.interpolatedName
    url[`${result.scale}x`] = imgURL
    srcSetItems.push(`${imgURL} ${result.scale}x`)
  }

  const outputData = {
    width: Math.floor(meta.width * (1 / options.baseScale)),
    height: Math.floor(meta.height * (1 / options.baseScale)),
    url,
    srcSet: srcSetItems.join(', '),
  }
  return `module.exports = ${JSON.stringify(outputData)}`
}

const processImageCb = callbackify(processImage)

export default function downscale(inputBuffer) {
  this.cacheable()
  const cb = this.async()
  processImageCb.call(this, inputBuffer, cb)
}

export const raw = true
