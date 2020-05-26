package racy

import math.{sqrt, cos}
import scala.util.Random
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector
import me.tongfei.progressbar.ProgressBar

object main extends App {
  def color(ray: Ray, world: Hitable, depth: Int): Color = {
    world.hit(ray, 0.0001) match {
      case Some(record) => {
        val emitColor = record.m.emitted(0, 0, record.p)

        record.m.scatter(ray, record) match {
          case Some(s) if depth < 50 =>
            emitColor + s.attenuation * color(s.scattered, world, depth + 1)
          case _ => emitColor
        }
      }
      case None => Color.black
    }
  }

  val red = Lambertian(Solid(Color(0.65, 0.05, 0.05)))
  val green = Lambertian(Solid(Color(0.12, 0.45, 0.15)))
  val white = Lambertian(Solid(Color(0.73, 0.73, 0.73)))
  val light = DiffuseLight(Color.white * 15.0)

  val cornellBox = List(
    Flip(YZRect(0, 0, 555, 555, 555, green)),
    YZRect(0, 0, 555, 555, 0, red),
    XZRect(213, 227, 343, 332, 554, light),
    Flip(XZRect(0, 0, 555, 555, 555, white)),
    XZRect(0, 0, 555, 555, 0, white),
    Flip(XYRect(0, 0, 555, 555, 555, white)),
    Sphere(Point(100, 50, 50), 50, Dielectrics(1.0))
  )

  val world = BVH.of(cornellBox).get

  val nx = 512
  val ny = 512
  val ns = 512

  val img = new Image(nx, ny)

  val camera = Camera(
    Point(278, 278, -800),
    Point(278, 278, -0.0),
    Vector.Y,
    40.0,
    nx.toDouble / ny.toDouble
  )

  val random = new Random()

  val ys = ParVector.range(0, ny)
  val xs = ParVector.range(0, nx)

  val progressBar = new ProgressBar("rendering", nx * ny)

  for (x <- xs;
       y <- ys) {
    img.put(
      x,
      ny - y - 1,
      scala.Vector
        .range(0, ns)
        .map { _ =>
          val u = (x.toDouble + random.nextDouble()) / nx
          val v = (y.toDouble + random.nextDouble()) / ny

          val r = camera(u, v)
          color(r, world, 0) / ns
        }
        .reduce(_ + _)
        .gamma
    )
    progressBar.step()
  }

  progressBar.close()

  img.save("./output.ppm")
}
