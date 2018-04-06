import ga.{Genotype, Overseer, Population}
import util.console

object Auto extends App {
  var overseer = new Overseer()
  var population: Population = overseer.getRandomPopulation

  promptAction

  def promptAction: Unit = {
    println(
      """
        |Select action
        |1. Genetic algorithm
        |2. Set constraints
        |3. Set parameters
        |4. Exit
      """.stripMargin
    )

    console.getInt(1 to 4) match {
      case 1 => promptGA
      case 2 => overseer = overseer.setConstraints
      case 3 => {
        overseer = overseer.setParameters
        population = population.resize(overseer.populationSize)
      }
      case _ => System.exit(0)
    }

    promptAction
  }

  def promptGA: Unit = {
    println(
      s"""
        |Current population:$population
        |
        |Select action
        |1. Run n times
        |2. Run until satisfactory result found
        |3. Reset
        |4. Return
      """.stripMargin
    )
    val action = console.getInt(1 to 4)
    action match {
      case 1 => {
        println("How many times you'd like to run GA for the current population?")
        population = overseer.runGA(console.getInt(), population)
      }
      case 3 => population = overseer.getRandomPopulation
      case _ =>
    }

    if (action < 4) promptGA
  }

}
