import ga.{Overseer, Population}
import util.console

object Auto extends App {
  var overseer = new Overseer()
  var population: Population = overseer.getRandomPopulation

  promptAction

  def promptAction: Unit = {
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
      case 1 => promptGA
      case 2 => overseer = overseer.promptManageVariables
      case 3 => {
        overseer = overseer.promptSetParameters
        population = population.resize(overseer.populationSize)
      }
      case _ => System.exit(0)
    }

    promptAction
  }

  def promptGA: Unit = {
    println(
      s"""
        |Nykyinen populaatio:$population
        |
        |Valitse toiminta
        |1. Suorita algoritmi N kertaa
        |2. Suorita kunnes tyydyttävä tulos löytyy
        |3. Nollaa populaatio
        |4. Palaa takaisin
      """.stripMargin
    )
    val action = console.getInt(1, 4)
    action match {
      case 1 => {
        population = overseer.runGA(
          console.getInt(query = Some("Kuinka monta kertaa GA suoritetaan?")),
          population
        )
      }
      case 3 => population = overseer.getRandomPopulation
      case _ =>
    }

    if (action < 4) promptGA
  }

}
