package racy

import math.tan

case class Camera(
    val lowerLeft: Point = Point(-2.0, -1.0, -1.0),
    val horizontal: Vector = Vector(4.0, 0.0, 0.0),
    val vertical: Vector = Vector(0.0, 2.0, 0.0),
    val origin: Point = Point(0.0, 0.0, 0.0)
) {
  def apply(u: Double, v: Double) =
    Ray(
      origin,
      (lowerLeft + (horizontal * u) + (vertical * v)) - origin
    )
}

object Camera {
  def apply(fov: Double, aspect: Double): Camera = {
    val theta = fov.toRadians
    val halfHeight = tan(theta / 2.0)
    val halfWidth = aspect * halfHeight

    Camera(
      lowerLeft = Point(-halfWidth, -halfHeight, -1.0),
      horizontal = Vector(2.0 * halfWidth, 0.0, 0.0),
      vertical = Vector(0.0, 2.0 * halfHeight, 0.0)
    )
  }

  def apply(
      lookFrom: Point,
      lookAt: Point,
      up: Vector,
      fov: Double,
      aspect: Double
  ): Camera = {
    val theta = fov.toRadians
    val halfHeight = tan(theta / 2.0)
    val halfWidth = aspect * halfHeight

    val w = (lookFrom - lookAt).unit
    val u = up.cross(w).unit
    val v = w.cross(u)

    val origin = lookFrom

    Camera(
      lowerLeft = origin - u * halfWidth - v * halfHeight - w,
      horizontal = u * 2.0 * halfWidth,
      vertical = v * 2.0 * halfHeight,
      origin = origin
    )
  }
}
