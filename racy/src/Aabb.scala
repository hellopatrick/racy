package racy

case class Aabb(
    val min: Point,
    val max: Point
) {
  def hit(ray: Ray, tAbsoluteMin: Double, tAbsoluteMax: Double): Boolean = {
    var tMin = tAbsoluteMin
    var tMax = tAbsoluteMax

    for (a <- 0 until 3) {
      val inv = 1.0 / ray.direction(a)

      val (t0, t1) = if (inv >= 0.0) {
        ((min(a) - ray.origin(a)) * inv, (max(a) - ray.origin(a)) * inv)
      } else {
        ((max(a) - ray.origin(a)) * inv, (min(a) - ray.origin(a)) * inv)
      }

      tMin = if (t0 > tMin) {
        t0
      } else {
        tMin
      }

      tMax = if (t1 < tMax) {
        t1
      } else {
        tMax
      }

      if (tMax <= tMin) { return false }
    }

    return true
  }

  def merge(that: Aabb): Aabb = {
    val minMerge = Point(
      Math.min(min.x, that.min.x),
      Math.min(min.y, that.min.y),
      Math.min(min.z, that.min.z)
    )

    val maxMerge = Point(
      Math.max(max.x, that.max.x),
      Math.max(max.y, that.max.y),
      Math.max(max.z, that.max.z)
    )

    Aabb(minMerge, maxMerge)
  }
}

object Aabb {
  val empty = Aabb(Point.ORIGIN, Point.ORIGIN)
}
