package util

import scala.annotation.tailrec
import scala.util.Random

object misc {
  @tailrec
  def randomIntExclude(n: Int, exclude: Set[Int]): Int =
    Random.nextInt(n) match {
      case r if exclude.contains(r) => randomIntExclude(n, exclude)
      case r => r
    }

  def randomIntExclude(n: Int, exclude: Int): Int = randomIntExclude(n, Set(exclude))

  @tailrec
  def randomDoubleExclude(exclude: Set[Double]): Double =
    Random.nextDouble match {
      case r if exclude.contains(r) => randomDoubleExclude(exclude)
      case r => r
    }

  def randomDoubleExclude(exclude: Double): Double = randomDoubleExclude(Set(exclude))

  // ei nykyisellään käytössä
  def getDistance(a: Seq[Double], b: Seq[Double]): Double =
    Math.sqrt(a.zip(b).map(i => Math.pow(i._1 - i._2, 2)).sum)

  // Normaalijakautunut satunnaisluku
  def getGaussianRandom(mean: Float = 0, standardDeviation: Float = .10f): Float = (
    mean +
      standardDeviation *
        Math.sqrt(-2 * Math.log(randomDoubleExclude(0))) * Math.cos(2 * Math.PI * randomDoubleExclude(0))
    ).toFloat
}
