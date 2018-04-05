package ga

import util.console

/**
  * Hallinnoi geneettisen algoritmin parametreja
  *
  * @param step
  * @param populationSize
  * @param maxCrossoversPerGeneration
  * @param maxMutationsPerGeneration
  */
case class Overseer (
  step: Float = .1f,
  populationSize: Int = 300,
  maxCrossoversPerGeneration: Int = 150,
  maxMutationsPerGeneration: Int = 100,
) {
  def setParameters: Overseer = {
    println(toString)
    println(
      """
        |Select which parameter you'd like to adjust
        |1. Step
        |2. Population Size
        |3. Maximum crossovers per generation
        |4. Maximum mutations per generation
        |5. I'm done
      """.stripMargin
    )
    console.getInteger(1 to 5) match {
      case 1 => copy(step = console.getFloat()).setParameters
      case 2 => copy(populationSize = console.getInteger()).setParameters
      case 3 => copy(maxCrossoversPerGeneration = console.getInteger()).setParameters
      case 4 => copy(maxMutationsPerGeneration = console.getInteger()).setParameters
      case _ => this
    }
  }

  override def toString: String =
    s"""
       |Parameters:
       |\tStep: ${step}
       |\tPopulation size: ${populationSize}
       |\tMaximum crossovers per generation: ${maxCrossoversPerGeneration}
       |\tMaximum mutations per generation: ${maxMutationsPerGeneration}
     """.stripMargin
}
