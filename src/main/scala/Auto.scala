import ga._
import util.console

import scala.annotation.tailrec

object Auto extends App {
  var overseer: Option[Overseer] = None
  var population: Option[Population] = None

  promptAction()

  def promptOverseerSelection(): Unit = {
    println("Valitse geneettisen algoritmin kohde")
    Overseer.overseers.zipWithIndex.foreach { case (overseer, index) =>
      println(s"${index + 1}. ${overseer.description}")
    }
    overseer = Some(Overseer.overseers(console.getInt(1, Overseer.overseers.length) - 1))
  }

  @tailrec
  def promptAction(): Unit = {
    if (overseer.isEmpty)
      promptOverseerSelection()
    else {
      println(
        """
          |Valitse toiminta
          |1. Geneettinen algoritmi
          |2. Hallinnoi muuttujia
          |3. Hallinnoi parametreja
          |4. Valitse toinen sovelluskohde
          |5. Lopeta
        """.stripMargin
      )

      console.getInt(1, 5) match {
        case 1 => promptGA()
        case 2 =>
          val newOverseer = overseer.get.promptManageVariables
          if (newOverseer.variables != overseer.get.variables) population = None
          overseer = Some(newOverseer)
        case 3 =>
          overseer = Some(overseer.get.promptSetParameters)
          if (population.isDefined) population = Some(population.get.resize(overseer.get.populationSize))
        case 4 =>
          promptOverseerSelection()
        case _ => System.exit(0)
      }
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
          overseer.get.runGA(console.getInt(query = Some("Kuinka monta kertaa GA suoritetaan?")), population)
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
