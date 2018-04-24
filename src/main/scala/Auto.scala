import ga.{Overseer, Phenotype, Population, Variable}
import util.console

object Auto extends App {
  var overseer = Overseer(
    fitnessFunction = (phenotype: Phenotype, variables: Seq[Variable]) => {
      val x = phenotype.genes(variables.head)
      val y = phenotype.genes(variables.head)
      y * Math.sin(Math.sqrt(x*x+y*y)) / Math.sqrt(x*x+y*y) + y*Math.signum(x)
    },
    Seq(
      Variable("x", "", -5, 5),
      Variable("y", "", -5, 5),
//      Variable("Ympäristön alkulämpötila", "ºC", 25, 25),
//      Variable("Puuaineksen tilavuus", "m^3", 0, 50),
//      Variable("Polttoastian tilavuus", "m^3", 0, 50),
//      Variable("Savukaasujen poistumisvirtaus", "m^3 / s", 0, 5),
//      Variable("Ilman sisäänvirtaus", "m^3 / s", 0, 5)
    )
  )
  var population: Option[Population] = None

  promptAction()

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

  def promptGA(): Unit = {
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
      case 1 =>
        population = overseer.runGA(console.getInt(query = Some("Kuinka monta kertaa GA suoritetaan?")))
      case 3 => population = None
      case _ =>
    }

    if (action < 4) promptGA()
  }

}
