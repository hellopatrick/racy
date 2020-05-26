package racy

case class Flip(hitable: Hitable) extends Hitable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    hitable.hit(ray, tMin, tMax) match {
      case Some(record) => {
        Some(HitRecord(record.t, record.p, record.n * -1.0, record.m))
      }
      case None => None
    }
  }

  val boundingBox: Aabb = hitable.boundingBox
}
