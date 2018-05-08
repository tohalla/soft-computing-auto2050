package ga

import util.console

import scala.annotation.tailrec
import scala.util.Random

case class Overseer(
  description: String,
  getFitnessValue: (Phenotype, Seq[Variable]) => Double, // jossa isompi kelvollisuusarvo = suurempi todennäköisyys
  // siirtää informaatiota seuraavaan populaatioon
  variables: Seq[Variable],
  parentSelection: ParentSelection = RankSelection,
  populationSize: Int = 80,
  elitism: Int = 1, // kuinka monta parasta ratkaisua siiretään suoraan seuraavaan populaatioon
  mutationProbability: Float = .08f,
  crossOverProbability: Float = .8f
) {
  @tailrec
  final def promptSetParameters: Overseer = {
    println(toString)
    println(
      """
        |Valitse asetettava parametri
        |1. Populaation koko (populaation koon muuttaminen saattaa muuttaa nykyistä populaatiota)
        |2. Risteytyksen todennäköisyys
        |3. Mutaation todennäköisyys
        |4. Elitismi
        |5. Valmis
      """.stripMargin
    )
    console.getInt(1, 5) match {
      case 1 => copy(populationSize = console.getInt()).promptSetParameters
      case 2 => copy(crossOverProbability = console.getFloat(0, 1)).promptSetParameters
      case 3 => copy(mutationProbability = console.getFloat(0, 1)).promptSetParameters
      case 4 => copy(elitism = console.getInt(maxValue = Some(populationSize))).promptSetParameters
      case _ => this
    }
  }

  @tailrec
  final def promptManageVariables: Overseer = {
    println(toString)
    println(
      s"""
         |Valitse muokattava muuttuja (muuttujien muokkaus nollaa nykyisen populaation)
         |${variables.zipWithIndex.map(c => s"${c._2 + 1}. ${c._1.text}").mkString("\n")}
         |${variables.length + 1}. Valmis
      """.stripMargin
    )
    console.getInt(1, variables.length + 1) match {
      case x if x < variables.length + 1 => copy(
        variables = {
          val min = console.getFloat(query = Some("Aseta pienin sallittu arvo"))
          variables.updated(
            x - 1,
            variables(x - 1).updated(
              min,
              console.getFloat(minValue = Some(min), query = Some("Aseta suurin sallittu arvo"))
            )
          )
        }
      ).promptManageVariables
      case _ => this
    }
  }

  // Funktio geneettisen algoritmin suorittamiseen N kertaa, palauttaa viimeisen populaation
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
       |\tElitismi: $elitism
       |Muuttujat
       |\t${
      variables.mkString("\n\t")
    }
     """.stripMargin

  private def genotypesWithUpdatedFitnessValue(genotypes: Vector[Genotype]): Vector[Genotype] =
    genotypes.map(genotype => genotype.copy(fitnessValue = getFitnessValue(genotype.decode, variables)))

  //
  private def crossover(parents: (Genotype, Genotype)): Vector[Genotype] =
    if (parents._1.size != parents._2.size || crossOverProbability < Random.nextFloat)
      Vector(parents._1, parents._2)
    else {
      val genes = parents._1.alleles.keys.foldLeft((parents._1.alleles, parents._2.alleles))((a, c) =>
        if (Random.nextBoolean) (
          a._1.updated(c, a._2(c)),
          a._2.updated(c, a._1(c))
        ) else a
      )
      Vector(Genotype(genes._1), Genotype(genes._2))
    }

  // Luo annetusta populaatiosta uuden populaation
  private def generateNewPopulation(population: Option[Population] = None): Option[Population] =
    if (population.isEmpty) Population.generatePopulation(populationSize, variables, Some(getFitnessValue))
    else {
      val candidates = genotypesWithUpdatedFitnessValue(population.get.genotypes)
        .sortWith(_.fitnessValue > _.fitnessValue)
      val parentCount = Math.ceil((populationSize - elitism) / 2f).toInt

      Some(
        new Population(
          genotypes = (
            (if (elitism > 0) candidates.take(elitism) else Vector.empty) ++
              parentSelection.getParents(candidates, parentCount).flatMap(crossover)
                .map(_.mutate(mutationProbability)) // mutaatio
            ).take(populationSize)
        )
      )
    }
}

object Overseer {
  // Sovelluksen valmiit valvoja -luokan oliot
  val overseers = Vector(
    Overseer(
      description = "y * sin(sqrt(x^2 + y^2)) + x * sign(y)",
      getFitnessValue = (phenotype: Phenotype, variables: Seq[Variable]) => {
        val x = phenotype.alleles(variables.head)
        val y = phenotype.alleles(variables(1))
        y * Math.sin(Math.sqrt(x * x + y * y)) + x * Math.signum(y)
      },
      variables = Seq(
        Variable("x", "", -20, 20),
        Variable("y", "", -20, 20)
      ),
      parentSelection = RankedTournamentSelection,
      populationSize = 50,
      mutationProbability = .25f
    ),
    Overseer(
      description = "a - b + c - d + e - f + g - h",
      getFitnessValue = (phenotype: Phenotype, variables: Seq[Variable]) =>
        phenotype.alleles(variables(0)) - phenotype.alleles(variables(1)) +
          phenotype.alleles(variables(2)) - phenotype.alleles(variables(3)) +
          phenotype.alleles(variables(4)) - phenotype.alleles(variables(5)) +
          phenotype.alleles(variables(6)) - phenotype.alleles(variables(7)),
      variables = Seq(
        Variable("a", "", 0, 20),
        Variable("b", "", 0, 20),
        Variable("c", "", 0, 20),
        Variable("d", "", 0, 20),
        Variable("e", "", 0, 20),
        Variable("f", "", 0, 20),
        Variable("g", "", 0, 20),
        Variable("h", "", 0, 20)
      ),
      parentSelection = RankSelection,
      populationSize = 75
    )
  )
}