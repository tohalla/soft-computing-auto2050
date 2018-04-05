package ga

import util.console

import scala.io.StdIn

case class Constraint(value: Any, text: String, unit: String, limit: Range = null)

case class Overseer(
  step: Float = .1f,
  populationSize: Int = 300,
  maxCrossoversPerGeneration: Int = 150,
  maxMutationsPerGeneration: Int = 100,
  constraints: Vector[Constraint] = Vector(
    new Constraint(0, "Time", "s"),
    new Constraint(0f, "Wood fuel volume", "m^3"),
    new Constraint(0f, "Furnace volume", "m^3"),
    new Constraint(0f, "Water content", null, 0 to 1)
  )
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

  def setConstraints: Overseer = {
    println(toString)
    println(
      s"""
         |Select which parameter you'd like to adjust
         |${
        constraints.zipWithIndex.map(c =>
          s"${c._2 + 1}. ${c._1.text}" +
            s"${if (c._1.limit != null) s" (value between ${c._1.limit.start} and ${c._1.limit.end}}" else ""}"
        ).mkString("\n")
      }
         |${constraints.length + 1}. I'm done
      """.stripMargin
    )
    console.getInteger(1 to constraints.length + 1) match {
      case x if x < constraints.length + 1 => copy(
        constraints = constraints.updated(
          x - 1,
          constraints(x - 1).copy(
            value = constraints(x - 1).value match {
              case _: Int => console.getInteger()
              case _: Float => console.getFloat()
              case _ => StdIn.readLine
            }
          )
        )
      ).setConstraints
      case _ => this
    }
  }

  override def toString: String =
    s"""
       |Parameters
       |\tStep: ${step}
       |\tPopulation size: ${populationSize}
       |\tMaximum crossovers per generation: ${maxCrossoversPerGeneration}
       |\tMaximum mutations per generation: ${maxMutationsPerGeneration}
       |Constraints
       |\t${
      constraints
        .map(c => s"${c.text}: ${if (c.value == 0) "Not set" else s"${c.value}${if (c.unit == null) "" else c.unit}"}")
        .mkString("\n\t")
    }
     """.stripMargin
}
