package racy

final case class XYRect(
    val x0: Double,
    val y0: Double,
    val x1: Double,
    val y1: Double,
    val z: Double,
    val material: Material
) extends Hitable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val t = (z - ray.origin.z) / ray.direction.z

    if (t < tMin || t > tMax) { None }
    else {
      val p = ray.apply(t)

      if (p.x < x0 || p.x > x1 || p.y < y0 || p.y > y1) { None }
      else { Some(HitRecord(t, p, Vector.Z, material)) }
    }
  }

  val boundingBox: Aabb =
    Aabb(Point(x0, y0, z - 0.0001), Point(x1, y1, z + 0.0001))

}

final case class XZRect(
    val x0: Double,
    val z0: Double,
    val x1: Double,
    val z1: Double,
    val y: Double,
    val material: Material
) extends Hitable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val t = (y - ray.origin.y) / ray.direction.y

    if (t < tMin || t > tMax) { None }
    else {
      val p = ray.apply(t)

      if (p.x < x0 || p.x > x1 || p.z < z0 || p.z > z1) { None }
      else { Some(HitRecord(t, p, Vector.Y, material)) }
    }
  }

  val boundingBox: Aabb =
    Aabb(Point(x0, y - 0.0001, z0), Point(x1, y + 0.0001, z1))

}

final case class YZRect(
    val y0: Double,
    val z0: Double,
    val y1: Double,
    val z1: Double,
    val x: Double,
    val material: Material
) extends Hitable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val t = (x - ray.origin.x) / ray.direction.x

    if (t < tMin || t > tMax) { None }
    else {
      val p = ray.apply(t)

      if (p.y < y0 || p.y > y1 || p.z < z0 || p.z > z1) { None }
      else { Some(HitRecord(t, p, Vector.X, material)) }
    }
  }

  val boundingBox: Aabb =
    Aabb(Point(x - 0.0001, y0, z0), Point(x + 0.0001, y1, z1))

}
