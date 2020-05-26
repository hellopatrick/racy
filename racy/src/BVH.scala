package racy

import scala.util.Random

case class BVH(left: Hitable, right: Hitable) extends Hitable {
  lazy val boundingBox = {
    left.boundingBox.merge(right.boundingBox)
  }

  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    boundingBox.hit(ray, tMin, tMax) match {
      case true => {
        val leftHit = left.hit(ray, tMin, tMax)
        val rightHit = right.hit(ray, tMin, tMax)
        (leftHit, rightHit) match {
          case (Some(a), Some(b)) if a.t <= b.t => leftHit
          case (Some(a), Some(b)) if a.t > b.t  => rightHit
          case (Some(a), None)                  => leftHit
          case (None, Some(b))                  => rightHit
          case (None, None)                     => None
        }
      }
      case false => { None }
    }
  }

  override def toString() = {
    val leftRep = if (left.isInstanceOf[BVH]) { left.toString }
    else { left.boundingBox.toString }

    val rightRep = if (right.isInstanceOf[BVH]) { right.toString }
    else { right.boundingBox.toString }

    s"BVH(${leftRep},${rightRep})"
  }
}

object BVH {
  def of(hitables: Seq[Hitable]): Option[BVH] = {
    val axis = Random.nextInt(3)

    hitables.sortWith { (a, b) =>
      axis match {
        case 0 => a.boundingBox.min.x < b.boundingBox.min.x
        case 1 => a.boundingBox.min.y < b.boundingBox.min.y
        case 2 => a.boundingBox.min.z < b.boundingBox.min.z
        case _ => true
      }
    } match {
      case Seq()            => None
      case Seq(single)      => Some(BVH(single, single))
      case Seq(left, right) => Some(BVH(left, right))
      case sorted: Seq[Hitable] => {
        val mid = sorted.length / 2

        val (left, right) = sorted.splitAt(mid)

        val leftNode = BVH.of(left)
        val rightNode = BVH.of(right)

        (leftNode, rightNode) match {
          case (Some(left), Some(right)) => Some(BVH(left, right))
          case _                         => None
        }
      }
    }
  }
}
