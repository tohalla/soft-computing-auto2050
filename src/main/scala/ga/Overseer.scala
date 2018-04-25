package ga

import util.{console, misc}

import scala.util.Random

case class Overseer(
  getFitnessValue: (Phenotype, Seq[Variable]) => Double,
  variables: Seq[Variable],
  selectionFunction: SelectionFunction = RankSelection,
  description: String = "",
  populationSize: Int = 300,
  maxCrossoversPerGeneration: Int = 300,
  maxMutationsPerGeneration: Int = 100,
  mutationProbability: Float = .15f
) {
  def promptSetParameters: Overseer = {
    println(toString)
    println(
      """
        |Valitse asetettava parametri
        |1. Populaation koko (adjusting population size will affect current population)
        |2. Sukupolven risteytysten enimmäismäärä
        |3. Sukupolven mutaatioiden enimmäismäärä
        |4. Mutaation todennäköisyys
        |5. Valmis
      """.stripMargin
    )
    console.getInt(1, 6) match {
      case 1 => copy(populationSize = console.getInt()).promptSetParameters
      case 2 => copy(maxCrossoversPerGeneration = console.getInt(0, populationSize)).promptSetParameters
      case 3 => copy(maxMutationsPerGeneration = console.getInt()).promptSetParameters
      case 4 => copy(mutationProbability = console.getFloat(0, 1)).promptSetParameters
      case _ => this
    }
  }

  def promptManageVariables: Overseer = {
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

  def runGA(iterations: Int, population: Option[Population] = None): Option[Population] =
    if (iterations > 0) runGA(iterations - 1, generateNewPopulation(population))
    else population

  override def toString: String =
    s"""
       |Parametrit
       |\tPopulaation koko: $populationSize
       |\tSukupolven risteytysten enimmäismäärä: $maxCrossoversPerGeneration
       |\tSukupolven mutaatioiden enimmäismäärä: $maxMutationsPerGeneration
       |\tMutaation todennäköisyys: $mutationProbability
       |Muuttujat
       |\t${
      variables.mkString("\n\t")
    }
     """.stripMargin

  private def generateNewPopulation(population: Option[Population] = None): Option[Population] = {
    if (population.isEmpty) return Population.generatePopulation(populationSize, variables.toSet)
    var mutations = 0
    var crossovers = 0

    def mutate(genotype: Genotype) =
      if (mutations < maxMutationsPerGeneration && mutationProbability >= Random.nextFloat) {
        mutations += 1
        genotype.mutate(variables(Random.nextInt(variables.length)))
      } else genotype

    val candidates = selectionFunction.getCandidates(population.get)  // select candidates from population
      .map(mutate) // ... and mutate

    Some(
      new Population(
        genotypes = Vector.fill(populationSize) {
          val r = Random.nextInt(candidates.length)
          if (crossovers < maxCrossoversPerGeneration) {
            crossovers += 1
            candidates(r).crossover(candidates(misc.randomIntExclude(candidates.length, r)))
          } else candidates(r) // otherwise pick random candidate to next population
        }
      )
    )
  }
}
