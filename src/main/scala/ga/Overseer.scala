package ga

import util.console
import util.misc

import scala.io.StdIn
import scala.util.Random

case class Constraint(value: Any, text: String, unit: String, limit: Range = null)

case class Overseer(
  step: Float = .1f,
  populationSize: Int = 300,
  maxCrossoversPerGeneration: Int = 300,
  maxMutationsPerGeneration: Int = 100,
  mutationProbability: Float = .3f,
  constraints: Vector[Constraint] = Vector(
    new Constraint(0, "Time", "s"),
    new Constraint(0f, "Wood fuel volume", "m^3"),
    new Constraint(0f, "Furnace volume", "m^3"),
    new Constraint(0f, "Water content", null, 0 to 1)
  )
) {
  def getRandomPopulation: Population = Population.generatePopulation(populationSize, 10)

  def setParameters: Overseer = {
    println(toString)
    println(
      """
        |Select which parameter you'd like to adjust
        |1. Step
        |2. Population Size (adjusting population size will affect current population)
        |3. Maximum crossovers per generation
        |4. Maximum mutations per generation
        |5. Mutation probability
        |6. I'm done
      """.stripMargin
    )
    console.getInt(1 to 6) match {
      case 1 => copy(step = console.getFloat()).setParameters
      case 2 => copy(populationSize = console.getInt()).setParameters
      case 3 => copy(maxCrossoversPerGeneration = console.getInt(0 to populationSize)).setParameters
      case 4 => copy(maxMutationsPerGeneration = console.getInt()).setParameters
      case 5 => copy(mutationProbability = console.getFloat(0 to 1)).setParameters
      case _ => this
    }
  }

  def setConstraints: Overseer = {
    println(toString)
    println(
      s"""
         |Select which constraint you'd like to adjust
         |${
        constraints.zipWithIndex.map(c =>
          s"${c._2 + 1}. ${c._1.text}" +
            s"${if (c._1.limit != null) s" (value between ${c._1.limit.start} and ${c._1.limit.end}}" else ""}"
        ).mkString("\n")
      }
         |${constraints.length + 1}. I'm done
      """.stripMargin
    )
    console.getInt(1 to constraints.length + 1) match {
      case x if x < constraints.length + 1 => copy(
        constraints = constraints.updated(
          x - 1,
          constraints(x - 1).copy(
            value = constraints(x - 1).value match {
              case _: Int => console.getInt()
              case _: Float => console.getFloat()
              case _ => StdIn.readLine
            }
          )
        )
      ).setConstraints
      case _ => this
    }
  }

  def runGA(iterations: Int, population: Population = Population.generatePopulation(populationSize, 10)): Population =
    if (iterations > 0) runGA(iterations - 1, getNextGeneration(population))
    else population

  private def getNextGeneration(population: Population): Population = {
    if (population == null) return population
    var mutations = 0
    var crossovers = 0

    def mutate(genotype: Genotype) =
      if (mutations < maxMutationsPerGeneration && mutationProbability >= Random.nextFloat) {
        mutations += 1
        genotype.mutate()
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

  override def toString: String =
    s"""
       |Parameters
       |\tStep: ${step}
       |\tPopulation size: ${populationSize}
       |\tMaximum crossovers per generation: ${maxCrossoversPerGeneration}
       |\tMaximum mutations per generation: ${maxMutationsPerGeneration}
       |\tMutation probability: ${mutationProbability}
       |Constraints
       |\t${
      constraints
        .map(c => s"${c.text}: ${if (c.value == 0) "Not set" else s"${c.value}${if (c.unit == null) "" else c.unit}"}")
        .mkString("\n\t")
    }
     """.stripMargin
}
