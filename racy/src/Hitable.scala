package racy

trait Hitable {
  def hit(
      ray: Ray,
      tMin: Double = 0,
      tMax: Double = Double.MaxValue
  ): Option[HitRecord]
}
