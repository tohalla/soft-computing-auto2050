import ga._
import util.console

import scala.annotation.tailrec

object Auto extends App {
    var overseer = Overseer(
      getFitnessValue = (phenotype: Phenotype, variables: Seq[Variable]) => {
        val x = phenotype.genes(variables.head)
        val y = phenotype.genes(variables(1))
        val len = Math.sqrt(x * x + y * y)
        y * Math.sin(len) * len / (len + 1) + y * Math.signum(x)
      },
      variables = Seq(
        Variable("x", "", -20, 20),
        Variable("y", "", -20, 20)
      ),
      selectionFunction = RankedTournamentSelection
    )
//  var overseer = Overseer(
//    getFitnessValue = (phenotype: Phenotype, variables: Seq[Variable]) =>
//      phenotype.genes(variables(0)) - phenotype.genes(variables(1)) +
//        phenotype.genes(variables(2)) - phenotype.genes(variables(3)) +
//        phenotype.genes(variables(4)) - phenotype.genes(variables(5)) +
//        phenotype.genes(variables(6)) - phenotype.genes(variables(7)),
//    variables = Seq(
//      Variable("a", "", 0, 10),
//      Variable("b", "", 0, 10),
//      Variable("c", "", 0, 10),
//      Variable("d", "", 0, 10),
//      Variable("e", "", 0, 10),
//      Variable("f", "", 0, 10),
//      Variable("g", "", 0, 10),
//      Variable("h", "", 0, 10)
//    ),
//    populationSize = 10
//  )
  var population: Option[Population] = None

  promptAction()

  @tailrec
  def promptAction(): Unit = {
    println(
      """
        |Valitse toiminta
        |1. Geneettinen algoritmi
        |2. Hallinnoi muuttujia
        |3. Hallinnoi parametreja
        |4. Lopeta
      """.stripMargin
    )

    console.getInt(1, 4) match {
      case 1 => promptGA()
      case 2 =>
        val newOverseer = overseer.promptManageVariables
        if (newOverseer.variables != overseer.variables) population = None
        overseer = newOverseer
      case 3 =>
        overseer = overseer.promptSetParameters
        if (population.isDefined) population = Some(population.get.resize(overseer.populationSize))
      case _ => System.exit(0)
    }

    promptAction()
  }

  @tailrec
  def promptGA(printPopulation: Boolean = true): Unit = {
    if (population.isDefined && printPopulation) println(s"Nykyinen populaatio: ${population.get}")
    println(
      s"""
         |Valitse toiminta
         |1. Suorita algoritmi N kertaa
         |2. Tulosta koko populaatio
         |3. Nollaa populaatio
         |4. Palaa takaisin
        """.stripMargin
    )
    val action = console.getInt(1, 4)
    action match {
      case 1 =>
        population = Some(
          overseer.runGA(console.getInt(query = Some("Kuinka monta kertaa GA suoritetaan?")), population)
        )
      case 2 =>
        if (population.isDefined)
          println(population.get.toString(false))
        else
          println("Populaatiota ei ole alustettu")
      case 3 =>
        population = None
      case _ =>
    }
    if (action < 4) promptGA(action != 2)
  }

}
