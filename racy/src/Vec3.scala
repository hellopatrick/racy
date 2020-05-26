package racy

trait Vec3 {
  val x: Double
  val y: Double
  val z: Double

  def apply(index: Int) = {
    index match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => Float.NaN
    }
  }
}
