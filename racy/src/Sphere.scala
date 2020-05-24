package racy

import math.sqrt

final case class Sphere(
    val center: Point,
    val radius: Double,
    val material: Material
) extends Hitable {
  override def hit(
      ray: Ray,
      minT: Double = 0,
      maxT: Double = Double.MaxValue
  ): Option[HitRecord] = {
    val offset = ray.origin - center

    val a = ray.direction.dot(ray.direction)
    val b = 2.0f * offset.dot(ray.direction)
    val c = offset.dot(offset) - radius * radius

    val discriminant = b * b - 4 * a * c

    if (discriminant >= 0) {
      val t = (-b - sqrt(discriminant)) / (2.0 * a)

      if (minT < t && t < maxT) {
        val p = ray(t)
        val n = (p - center) / radius
        return Some(HitRecord(t, p, n, material))
      }

      val s = (-b + sqrt(discriminant)) / (2.0 * a)

      if (minT < s && s < maxT) {
        val p = ray(s)
        val n = (p - center) / radius
        return Some(HitRecord(s, p, n, material))
      }
    }

    None
  }
}
