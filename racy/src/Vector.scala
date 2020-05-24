package racy

import math.sqrt
import scala.util.Random

final case class Vector(val x: Double, val y: Double, val z: Double)
    extends Vec3 {

  def unary_-() = {
    Vector(-x, -y, -z)
  }

  def +(that: Vector) = {
    Vector(that.x + x, that.y + y, that.z + z)
  }

  def -(that: Vector) = {
    Vector(x - that.x, y - that.y, z - that.z)
  }

  def *(s: Double) = {
    Vector(s * x, s * y, s * z)
  }

  def /(s: Double) = {
    Vector(x / s, y / s, z / s)
  }

  def length = {
    sqrt(x * x + y * y + z * z)
  }

  lazy val unit = this / length

  def dot(that: Vector) =
    (x * that.x) + (y * that.y) + (z * that.z)

  def cross(that: Vector): Vector = Vector(
    y * that.z - that.y * z,
    z * that.x - that.z * x,
    x * that.y - that.x * y
  )

}

object Vector {
  val X = Vector(1.0, 0.0, 0.0)
  val Y = Vector(0.0, 1.0, 0.0)
  val Z = Vector(0.0, 0.0, 1.0)

  def randomInUnitCircle() = {
    val r = new Random()

    val x = r.nextGaussian()
    val y = r.nextGaussian()
    val z = r.nextGaussian()

    Vector(x, y, z).unit
  }
}
