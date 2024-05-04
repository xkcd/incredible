import type { ColliderDesc } from '@dimforge/rapier2d'
import { coords } from '../../../lib/coords'
import { Basis, RandallPath } from '../../../lib/utils'

export function lineCuboid(
  c: typeof ColliderDesc,
  { x1, x2, y1, y2, thickness }: RandallPath,
  { xBasis, yBasis, scale = 1 }: Basis,
): ColliderDesc {
  const width = Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2) / scale
  const height = thickness / scale
  const rotation = -Math.atan2(y2 - y1, x2 - x1)

  return c
    .cuboid(...coords.toRapier.lengths(width / 2, height / 2))
    .setTranslation(
      ...coords.toRapier.vector(
        ((x1 + x2) / 2 - xBasis) / scale,
        ((y1 + y2) / 2 - yBasis) / scale,
      ),
    )
    .setRotation(rotation)
}
