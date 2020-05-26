package racy

trait Texture {
  def apply(u: Double, v: Double, p: Point): Color
}

case class Solid(color: Color) extends Texture {
  def apply(u: Double, v: Double, p: Point): Color = color
}

case class Checker(off: Texture, on: Texture) extends Texture {
  def apply(u: Double, v: Double, p: Point): Color = {
    val loc = Math.sin(10 * p.x) * Math.sin(10 * p.y) * Math.sin(10 * p.z)

    if (loc < 0) {
      off(u, v, p)
    } else {
      on(u, v, p)
    }
  }
}
