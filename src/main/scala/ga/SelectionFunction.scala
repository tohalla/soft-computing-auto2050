package ga

import scala.util.Random

trait SelectionFunction {
  def getCandidates(population: Population): Vector[Genotype]

  protected def getSurvivalProbability(genotype: Genotype, averageGenotype: Option[Genotype]): Float

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
}

object RankSelection extends SelectionFunction {
  def getSurvivalProbability(genotype: Genotype, averageGenotype: Option[Genotype]) = 0f

  override def getCandidates(population: Population): Vector[Genotype] =
    sort(population.genotypes)
      .zipWithIndex
      .foldLeft(Vector.empty[Genotype])((a, c) =>
        if (1f / (2f + c._2) >= Random.nextFloat) a :+ c._1 else a
      )

}