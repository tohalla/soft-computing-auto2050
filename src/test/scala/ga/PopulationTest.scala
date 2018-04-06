package ga

import org.scalatest.FunSuite

class PopulationTest extends FunSuite {
  val population = Population.generatePopulation(100, 10)

  test("should resize population to smaller value correctly") {
    assert(population.genotypes.take(20) == population.resize(20).genotypes)
  }

  test("should resize population to greater value correctly") {
    assert(population.genotypes == population.resize(300).genotypes.take(100))
  }
}
