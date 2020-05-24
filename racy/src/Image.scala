package racy

import java.io._

class Image(val dimX: Int, val dimY: Int) {
  val buffer: Array[Array[Color]] = Array.ofDim[Color](dimY, dimX)

  def put(x: Int, y: Int, color: Color) = {
    buffer(y)(x) = color
  }

  def save(filename: String) = {
    val file = new File(filename)

    val bw = new BufferedWriter(new FileWriter(file))

    bw.write(s"P3\n$dimX $dimY\n255\n")

    for (row <- buffer;
         color <- row) {
      val (r, g, b) = color.pixel
      bw.write(s"$r $g $b\n")
    }

    bw.close()
  }
}
