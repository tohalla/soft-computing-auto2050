import ga.Overseer
import util.console

object Auto extends App {
  var overseer = new Overseer()

  selectAction()

  def selectAction(): Unit = {
    println(
      """
        |Select action
        |1. Run genetic algorithm
        |2. Set parameters
        |3. Exit
      """.stripMargin
    )

    console.getInteger(1 to 3) match {
      case 2 => overseer = overseer.setParameters
      case _ => System.exit(0)
    }

    selectAction()
  }

}
