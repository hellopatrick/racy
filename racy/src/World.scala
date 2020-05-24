package racy

final case class World(val hitables: Seq[Hitable]) extends Hitable {
  override def hit(
      ray: Ray,
      tMin: Double = 0,
      tMax: Double = Double.MaxValue
  ): Option[HitRecord] =
    hitables.flatMap { _.hit(ray, tMin, tMax) } match {
      case Seq() => { None }
      case hits  => Some(hits.minBy { _.t })
    }
}
