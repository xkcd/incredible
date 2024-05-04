import imgCushion from '@art/cushion_4x.png'
import { coords } from '../../lib/coords'
import { Angled, Vector } from '../../types'
import { ComicImage } from '../ComicImage'
import { EditableWidget, useSelectHandlers } from '../MachineTileEditor'
import { useCollider } from '../PhysicsContext'
import { getPositionStyles } from '../positionStyles'

export interface CushionWidget extends Vector, Angled {
  type: 'cushion'
}

export function CushionPreview() {
  return <ComicImage img={imgCushion} css={{ width: '70%', height: 'auto' }} />
}

export function Cushion({
  id,
  onSelect,
  x,
  y,
  angle,
}: CushionWidget & EditableWidget) {
  const width = imgCushion.width
  const height = imgCushion.height

  const widthRatio = width / 328
  const heightRatio = height / 157

  useCollider(
    ({ ColliderDesc, CoefficientCombineRule }) => {
      const vectorConversion = (xIn: number, yIn: number): [number, number] =>
        coords.toRapier.vector(
          -0.5 * width + xIn * widthRatio,
          -0.5 * height + yIn * heightRatio,
        )

      return ColliderDesc.roundConvexHull(
        new Float32Array([
          ...vectorConversion(6, 40),
          ...vectorConversion(112, 10),
          ...vectorConversion(214, 10),
          ...vectorConversion(321, 42),
          ...vectorConversion(317, 55),
          ...vectorConversion(320, 76),
          ...vectorConversion(315, 100),
          ...vectorConversion(322, 114),
          ...vectorConversion(220, 146),
          ...vectorConversion(139, 149),
          ...vectorConversion(10, 120),
          ...vectorConversion(13, 101),
          ...vectorConversion(12, 58),
        ]),
        coords.toRapier.length(5.5 * widthRatio),
      )!
        .setTranslation(...coords.toRapier.vector(x, y))
        .setRotation(coords.toRapier.angle(angle))
        .setRestitutionCombineRule(CoefficientCombineRule.Min)
        .setFrictionCombineRule(CoefficientCombineRule.Max)
        .setRestitution(0.0)
        .setFriction(0.999)
    },
    [angle, width, height, x, y, widthRatio, heightRatio],
  )

  return (
    <ComicImage
      {...useSelectHandlers(id, onSelect)}
      img={imgCushion}
      style={getPositionStyles(x, y, angle)}
    />
  )
}
