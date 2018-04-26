package ga

import scala.annotation.tailrec
import scala.util.Random

trait SelectionFunction {
  def getParents(candidates: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)]

  def prepare(genotypes: Vector[Genotype]): Vector[Genotype] = sort(genotypes)

  // normalizes fitness values to range of [0,1]
  protected def normalizeFitness(genotypes: Vector[Genotype]): Vector[Genotype] = {
    val sorted = sort(genotypes)
    sorted.map(genotype => genotype.copy(
      fitnessValue = (genotype.fitnessValue + Math.abs(sorted.last.fitnessValue)) /
        (sorted.head.fitnessValue + Math.abs(sorted.last.fitnessValue))
    ))
  }

  protected def sort(genotypes: Vector[Genotype]): Vector[Genotype] =
    genotypes.sortWith(_.fitnessValue > _.fitnessValue)

  @tailrec
  protected final def drawParent(candidatesWithProbabilities: Vector[(Genotype, Float)], i: Int = 0): Genotype =
    if (i < candidatesWithProbabilities.length - 1 && candidatesWithProbabilities(i)._2 < Random.nextFloat)
      drawParent(candidatesWithProbabilities, i + 1)
    else {
      candidatesWithProbabilities(i)._1
    }
}

object RankSelection extends SelectionFunction {
  override def getParents(genotypes: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)] = {
    val rankSum = (genotypes.length + 1) * genotypes.length / 2
    val candidatesWithProbabilities = genotypes.zip(Vector.tabulate(genotypes.length)(i => (i + 1f) / rankSum).reverse)
    Vector.fill(parentCount)(
      (drawParent(candidatesWithProbabilities), drawParent(candidatesWithProbabilities))
    )
  }
}

object RankedTournamentSelection extends SelectionFunction {
  val tournamentSize: Int = 4

  override def prepare(genotypes: Vector[Genotype]): Vector[Genotype] = genotypes

  override def getParents(genotypes: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)] = {
    val rankSum = (tournamentSize + 1) * tournamentSize / 2
    val probabilities = Vector.tabulate(tournamentSize)(i => (i + 1f) / rankSum)
    Vector.fill(parentCount)({
      val candidatesWithProbabilities = sort(Vector.fill(tournamentSize)(genotypes(Random.nextInt(genotypes.length))))
        .zip(probabilities)
      (drawParent(candidatesWithProbabilities), drawParent(candidatesWithProbabilities))
    })
  }

}

