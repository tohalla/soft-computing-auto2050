package ga

import util.{console, misc}

import scala.util.Random

case class Overseer(
  populationSize: Int = 300,
  maxCrossoversPerGeneration: Int = 300,
  maxMutationsPerGeneration: Int = 100,
  mutationProbability: Float = .3f,
  variables: Seq[Variable] = Seq(
    new Variable("Ympäristön alkulämpötila", "ºC", 25, 25),
    new Variable("Puuaineksen tilavuus", "m^3", 0, 50),
    new Variable("Polttoastian tilavuus", "m^3", 0, 50),
    new Variable("Savukaasujen poistumisvirtaus", "m^3 / s", 0, 5),
    new Variable("Ilman sisäänvirtaus", "m^3 / s", 0, 5),
    new Variable("Puun kosteuspitoisuus", null, .05f, .65f),
    new Variable("Hiilen osuus kuivasta puusta", null, 0.114f, 0.156f),
    new Variable("Vedyn osuus kuivasta puusta", null, 0.06f, 0.065f),
    new Variable("Hapen osuus kuivasta puusta", null, 0.38f, 0.42f),
    new Variable("Typen osuus kuivasta puusta", null, 0.001f, 0.005f),
    new Variable("Rikin osuus kuivasta puusta", null, .0005f, .0005f),
    new Variable("Tuhkan osuus kuivasta puusta", null, .004f, .006f)
  )
) {
  def getRandomPopulation: Population = Population.generatePopulation(populationSize, variables.toSet)

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

  def runGA(
    iterations: Int,
    population: Population = Population.generatePopulation(populationSize, variables.toSet)
  ): Population =
    if (iterations > 0) runGA(iterations - 1, getNextGeneration(population))
    else population

  override def toString: String =
    s"""
       |Parametrit
       |\tPopulaation koko: ${populationSize}
       |\tSukupolven risteytysten enimmäismäärä: ${maxCrossoversPerGeneration}
       |\tSukupolven mutaatioiden enimmäismäärä: ${maxMutationsPerGeneration}
       |\tMutaation todennäköisyys: ${mutationProbability}
       |Muuttujat
       |\t${
      variables.mkString("\n\t")
    }
     """.stripMargin

  private def getNextGeneration(population: Population): Population = {
    if (population == null) return population
    var mutations = 0
    var crossovers = 0

    def mutate(genotype: Genotype) =
      if (mutations < maxMutationsPerGeneration && mutationProbability >= Random.nextFloat) {
        mutations += 1
        genotype.mutate(variables(Random.nextInt(variables.length)))
      } else genotype

    val candidates = Random.shuffle(population.genotypes)
      .filter(_ => Random.nextFloat < .3f) // elimination, random elimination as placeholder
      .map(mutate)

    new Population(
      genotypes = Vector.fill(populationSize) {
        val r = Random.nextInt(candidates.length)
        if (crossovers < maxCrossoversPerGeneration) {
          crossovers += 1
          candidates(r).crossover(candidates(misc.randomIntExclude(candidates.length, r)))
        } else candidates(r) // otherwise pick random candidate to next population
      }
    )
  }
}
