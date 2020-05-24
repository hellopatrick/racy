package racy

import math.{sqrt, cos}
import scala.util.Random
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector

object main extends App {
  def color(ray: Ray, world: World, depth: Int): Color = {
    if (depth > 50) {
      return Color(0, 0, 0)
    }

    world.hit(ray, 0.0001) match {
      case Some(record) => {
        record.m.scatter(ray, record) match {
          case Some(s) => s.attenuation * color(s.scattered, world, depth + 1)
          case _       => Color(0, 0, 0)
        }
      }
      case None => {
        val t = 0.5 * (ray.direction.unit.y + 1.0)
        Color(1.0 - t, 1.0 - t, 1.0 - t) + Color(0.5, 0.7, 1.0)
      }
    }
  }

  val spheres = List(
    Sphere(Point(0.0, 0.0, -1.0), 0.5, Lambertian(Color(0.3, 0.3, 0.8))),
    Sphere(Point(0.0, -200.5, -1.0), 200.0, Lambertian(Color(0.8, 0.8, 0.0))),
    Sphere(Point(1.0, 0.0, -1.0), 0.5, Metal(Color(0.8, 0.6, 0.2), 1.0)),
    Sphere(Point(-1.0, 0.0, -1.0), 0.5, Dielectrics(1.5)),
    Sphere(Point(-1.0, 0.0, -1.0), -0.45, Dielectrics(1.5))
  )

  val world = World(spheres)

  val nx = 800
  val ny = 400
  val ns = 64

  val img = new Image(nx, ny)

  val camera = Camera(
    Point(0.0, 1.0, 1.0),
    Point(0.0, 0.0, -1.0),
    Vector.Y,
    90.0,
    nx.toDouble / ny.toDouble
  )

  val random = new Random()

  val ys = ParVector.range(0, ny)
  val xs = ParVector.range(0, nx)

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
  }

  img.save("./output.ppm")
}
