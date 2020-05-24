package racy

final case class Point(val x: Double, val y: Double, val z: Double)
    extends Vec3 {
  def -(that: Point) = {
    Vector(x - that.x, y - that.y, z - that.z)
  }

  def +(that: Vector) = {
    Point(x + that.x, y + that.y, z + that.z)
  }

  def -(that: Vector) = {
    Point(x - that.x, y - that.y, z - that.z)
  }
}

object Point {
  val ORIGIN = Point(0.0, 0.0, 0.0)
}
