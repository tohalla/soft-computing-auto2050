package ga

import org.scalatest.FunSuite

class PopulationTest extends FunSuite {
  val variables = Seq(
    Variable("", "", minValue = 0, maxValue = 1),
    Variable("", "", minValue = 0, maxValue = 10),
    Variable("", "", minValue = 1, maxValue = 1),
    Variable("", "", minValue = 0, maxValue = 30),
    Variable("", "", minValue = 0, maxValue = 30),
    Variable("", "", minValue = 0, maxValue = 30)
  )
  val population: Option[Population] = Population.generatePopulation(100, variables)

  test("should resize population to smaller value correctly") {
    assert(population.get.genotypes.take(20) == population.get.resize(20).genotypes)
  }

  test("should resize population to greater value correctly") {
    assert(population.get.genotypes == population.get.resize(300).genotypes.take(100))
  }
}
