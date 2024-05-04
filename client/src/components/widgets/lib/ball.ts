import { ColliderDesc } from '@dimforge/rapier2d'
import { coords } from '../../../lib/coords'
import { Basis } from '../../../lib/utils'

type BallFunc = typeof ColliderDesc.ball
/**
 * helpful function for converting paths you got from an image into a ball
 * @param create function that can create a ball
 * @param basis how to convert the provided values to a coordinate system in pixels centered at 0, 0
 * @param radius radius
 * @param center center of the ball
 */
export default function Ball(
  create: BallFunc,
  basis: Basis,
  radius: number,
  center: { x: number; y: number },
): ColliderDesc {
  const scale = basis.scale || 1
  //pointToVectorObject(basis, center.x, center.y)
  return create(coords.toRapier.length(radius / scale)).setTranslation(
    ...coords.toRapier.vector(center.x, center.y, basis),
  )
}
