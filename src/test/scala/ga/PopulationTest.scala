package ga

import org.scalatest.FunSuite

class PopulationTest extends FunSuite {
  val variables = Set(
    new Variable("", "", minValue = 0, maxValue = 1),
    new Variable("", "", minValue = 0, maxValue = 10),
    new Variable("", "", minValue = 1, maxValue = 1),
    new Variable("", "", minValue = 0, maxValue = 30),
    new Variable("", "", minValue = 0, maxValue = 30),
    new Variable("", "", minValue = 0, maxValue = 30)
  )
  val population = Population.generatePopulation(100, variables)

  test("should resize population to smaller value correctly") {
    assert(population.genotypes.take(20) == population.resize(20).genotypes)
  }

  test("should resize population to greater value correctly") {
    assert(population.genotypes == population.resize(300).genotypes.take(100))
  }
}
