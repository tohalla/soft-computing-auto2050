package ga

import util.console

import scala.annotation.tailrec
import scala.util.Random

case class Overseer(
  getFitnessValue: (Phenotype, Seq[Variable]) => Double,
  variables: Seq[Variable],
  selectionFunction: SelectionFunction = RankSelection,
  description: String = "",
  populationSize: Int = 300,
  elitism: Int = 1,
  mutationProbability: Float = .3f,
  crossOverProbability: Float = .8f
) {
  @tailrec
  final def promptSetParameters: Overseer = {
    println(toString)
    println(
      """
        |Valitse asetettava parametri
        |1. Populaation koko (adjusting population size will affect current population)
        |2. Risteytyksen todennäköisyys
        |4. Mutaation todennäköisyys
        |5. Valmis
      """.stripMargin
    )
    console.getInt(1, 6) match {
      case 1 => copy(populationSize = console.getInt()).promptSetParameters
      case 2 => copy(crossOverProbability = console.getFloat(0, 1)).promptSetParameters
      case 3 => copy(mutationProbability = console.getFloat(0, 1)).promptSetParameters
      case _ => this
    }
  }

  @tailrec
  final def promptManageVariables: Overseer = {
    println(toString)
    println(
      s"""
         |Valitse muokattava muuttuja (muuttujien muokkaus nollaa nykyisen populaation)
         |${
        variables.zipWithIndex.map(c =>
          s"${c._2 + 1}. ${c._1.text}"
        ).mkString("\n")
      }
         |${variables.length + 1}. Valmis
      """.stripMargin
    )
    console.getInt(1, variables.length + 1) match {
      case x if x < variables.length + 1 => copy(
        variables = variables.updated(
          x - 1,
          variables(x - 1)
            .updated(console.getFloat(Some(0)), console.getFloat(Some(0)))
        )
      ).promptManageVariables
      case _ => this
    }
  }

  @tailrec
  final def runGA(iterations: Int, population: Option[Population] = None): Population =
    if (iterations > 0) runGA(iterations - 1, generateNewPopulation(population))
    else {
      val p = population.getOrElse(generateNewPopulation(population).get)
      p.copy(genotypes = genotypesWithUpdatedFitnessValue(p.genotypes.sortWith(_.fitnessValue > _.fitnessValue)))
    }

  override def toString: String =
   s"""
       |Parametrit
       |\tPopulaation koko: $populationSize
       |\tRisteytyksen todennäköisyys: $crossOverProbability
       |\tMutaation todennäköisyys: $mutationProbability
       |Muuttujat
       |\t${
      variables.mkString("\n\t")
    }
     """.stripMargin

  private def genotypesWithUpdatedFitnessValue(genotypes: Vector[Genotype]): Vector[Genotype] =
    genotypes.map(genotype => genotype.copy(fitnessValue = getFitnessValue(genotype.decode, variables)))

  private def crossover(parents: (Genotype, Genotype)): Vector[Genotype] =
    if (parents._1.size != parents._2.size || crossOverProbability < Random.nextFloat)
      Vector(parents._1, parents._2)
    else {
      val genes = parents._1.genes.keys.foldLeft((parents._1.genes, parents._2.genes))((a, c) =>
        if (Random.nextBoolean) (
          a._1.updated(c, a._2(c)),
          a._2.updated(c, a._1(c))
        ) else a
      )
      Vector(Genotype(genes._1), Genotype(genes._2))
    }

  private def generateNewPopulation(population: Option[Population] = None): Option[Population] =
    if (population.isEmpty) Population.generatePopulation(populationSize, variables, Some(getFitnessValue))
    else {
      // prepare and update fitness value of parent candidates
      val candidates = selectionFunction.prepare(genotypesWithUpdatedFitnessValue(population.get.genotypes))
      val parentCount = Math.ceil((populationSize - elitism) / 2).toInt

      Some(
        new Population(
          genotypes = (
            (if (elitism > 0) candidates.take(elitism) else Vector.empty) ++
              Vector.fill(parentCount)(
                selectionFunction.getParents(population.get.genotypes, parentCount).flatMap(crossover)
              )
                .flatten
                // ... and mutate
                .map(genotype => if (mutationProbability >= Random.nextFloat) genotype.mutate else genotype)
            ).take(populationSize)
        )
      )
    }
}
