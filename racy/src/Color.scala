package racy

import math.{min, max, sqrt}

final case class Color(val r: Double, val g: Double, val b: Double) {
  lazy val gamma =
    Color(sqrt(r), sqrt(g), sqrt(b))

  lazy val pixel: (Int, Int, Int) = {
    val red = max(min((r * 255.0).toInt, 255), 0)
    val green = max(min((g * 255.0).toInt, 255), 0)
    val blue = max(min((b * 255.0).toInt, 255), 0)

    (red, green, blue)
  }

  def *(that: Color) = {
    Color(that.r * r, that.g * g, that.b * b)
  }

  def *(s: Double) = {
    Color(s * r, s * g, s * b)
  }

  def /(s: Double) = {
    Color(r / s, g / s, b / s)
  }

  def +(that: Color) = {
    Color(that.r + r, that.g + g, that.b + b)
  }
}
