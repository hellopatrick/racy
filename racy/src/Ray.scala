package racy

final case class Ray(origin: Point, direction: Vector) {
  def apply(t: Double): Point = {
    origin + (direction * t)
  }
}
