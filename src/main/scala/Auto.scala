import ga._
import util.console
import org.sameersingh.scalaplot.Implicits._

import scala.annotation.tailrec

object Auto extends App {
  var overseer: Option[Overseer] = None
  var visualization: Boolean = true
  var population: Option[Population] = None

  promptAction()

  def promptOverseerSelection(): Unit = {
    println("Valitse geneettisen algoritmin kohde")
    Overseer.overseers.zipWithIndex.foreach { case (overseer, index) =>
      println(s"${index + 1}. ${overseer.description}")
    }
    population = None
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
         |2. Graafinen seuranta: $visualization
         |3. Tulosta koko populaatio
         |4. Nollaa populaatio
         |5. Palaa takaisin
        """.stripMargin
    )
    val action = console.getInt(1, 5)
    action match {
      case 1 =>
        val iterations = console.getInt(query = Some("Kuinka monta kertaa GA suoritetaan?"))
        if (visualization) { // esitetään kuvaaja käyttäjälle
          val curves = (0 to iterations).foldLeft((Seq.empty[Double], Seq.empty[Double]))((a, i) => {
            population = Some(overseer.get.runGA(1, population))
            val genotypes = population.get.genotypes.sortWith(_.fitnessValue > _.fitnessValue)
            (
              a._1 :+ genotypes.head.fitnessValue,
              a._2 :+ genotypes.foldLeft(0d)(_ + _.fitnessValue) / population.get.size
            )
          })
          val x = 0 to iterations map(_.toDouble)
          output(GUI, xyChart(
            List(
              x -> Y(curves._1, "Paras arvo"),
              x -> Y(curves._2, "Keskiarvo")
            ),
            x = Axis(label = "Sykli"),
            y = Axis(label = "Kelvollisuusarvo"),
            showLegend = true
          ))
        }
        else population = Some(overseer.get.runGA(iterations, population))

        // ... lajitellaan vielä kelvollisuusarvon perusteella
        population = population.get.copy(genotypes = population.get.genotypes.sortWith(_.fitnessValue > _.fitnessValue))
      case 2 =>
        visualization = !visualization
      case 3 =>
        if (population.isDefined)
          println(population.get.toString(false))
        else
          println("Populaatiota ei ole alustettu")
      case 4 =>
        population = None
      case _ =>
    }
    if (action < 5) promptGA(action != 3)
  }

}
