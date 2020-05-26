package racy

import math.{sqrt, pow}
import scala.util.Random

case class Scatter(attenuation: Color, scattered: Ray)

trait Material {
  def scatter(ray: Ray, hitRecord: HitRecord): Option[Scatter]
  def emitted(u: Double, v: Double, p: Point): Color = Color.black
}

case class Lambertian(albedo: Texture) extends Material {
  override def scatter(
      ray: Ray,
      hitRecord: HitRecord
  ): Option[Scatter] = {
    val target =
      hitRecord.p + hitRecord.n + Vector.randomInUnitCircle()

    Some(
      Scatter(
        albedo(0, 0, hitRecord.p),
        Ray(hitRecord.p, target - hitRecord.p)
      )
    )
  }
}

case class Metal(albedo: Color, fuzziness: Double = 0.0) extends Material {
  def reflect(v: Vector, n: Vector) =
    v - n * 2.0 * v.dot(n)

  override def scatter(
      ray: Ray,
      hitRecord: HitRecord
  ): Option[Scatter] = {
    val reflected = reflect(ray.direction.unit, hitRecord.n)

    val scattered =
      Ray(hitRecord.p, reflected + Vector.randomInUnitCircle * fuzziness)

    if (scattered.direction.dot(hitRecord.n) > 0) {
      Some(
        Scatter(
          albedo,
          scattered
        )
      )
    } else {
      None
    }
  }
}

case class Dielectrics(refractionIndex: Double) extends Material {
  val random = new Random()

  def reflect(v: Vector, n: Vector) =
    v - n * 2.0 * v.dot(n)

  def refract(v: Vector, n: Vector, index: Double): Option[Vector] = {
    val u = v.unit
    val dt = u.dot(n)
    val discriminant = 1.0 - (index * index) * (1 - dt * dt)

    if (discriminant > 0.0) {
      Some(
        (u - n * dt) * index - n * sqrt(discriminant)
      )
    } else {
      None
    }
  }

  def schlick(c: Double): Double = {
    val r0 = (1.0 - refractionIndex) / (1.0 + refractionIndex)
    val r2 = r0 * r0

    r2 + (1.0 - r2) * pow(1.0 - c, 5.0)
  }

  def scatter(ray: Ray, hitRecord: HitRecord): Option[Scatter] = {
    val attenuation = Color(1.0, 1.0, 1.0)

    val (normal, index, cosine) = if (ray.direction.dot(hitRecord.n) > 0) {
      (
        -hitRecord.n,
        refractionIndex,
        (refractionIndex * ray.direction.dot(hitRecord.n)) / ray.direction.length
      )
    } else {
      (
        hitRecord.n,
        1.0 / refractionIndex,
        (-1.0 * ray.direction.dot(hitRecord.n)) / ray.direction.length
      )
    }

    refract(ray.direction, normal, index) match {
      case Some(vec) if schlick(cosine) <= random.nextDouble =>
        Some(Scatter(attenuation, Ray(hitRecord.p, vec)))
      case _ =>
        Some(
          Scatter(
            attenuation,
            Ray(hitRecord.p, reflect(ray.direction, hitRecord.n))
          )
        )
    }
  }
}

case class DiffuseLight(color: Color) extends Material {
  def scatter(ray: Ray, hitRecord: HitRecord): Option[Scatter] = None
  override def emitted(u: Double, v: Double, p: Point): Color = color
}
