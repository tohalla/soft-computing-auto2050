package util

import scala.util.Random

object misc {
  def randomIntExclude(n: Int, exclude: Set[Int]): Int =
    Random.nextInt(n) match {
      case r if exclude.contains(r) => randomIntExclude(n, exclude)
      case r => r
    }

  def randomIntExclude(n: Int, exclude: Int): Int = randomIntExclude(n, Set(exclude))

  def getDistance(a: Seq[Double], b: Seq[Double]): Double =
    Math.sqrt(a.zip(b).map(i => Math.pow(i._1 - i._2, 2)).sum)
}
