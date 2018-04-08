package ga

import org.scalatest.FunSuite

class EntityTest extends FunSuite {
  val variables = Set(
    new Variable("", "", minValue = 0, maxValue = 1),
    new Variable("", "", minValue = 0, maxValue = 10),
    new Variable("", "", minValue = 1, maxValue = 1),
    new Variable("", "", minValue = 0, maxValue = 30),
    new Variable("", "", minValue = 0, maxValue = 30),
    new Variable("", "", minValue = 0, maxValue = 30)
  )

  test("should do crossover correctly") {
    assert(
      new Genotype(variables.zip(Seq.tabulate(variables.size)(_.toFloat)).toMap)
        .crossover(
          new Genotype(variables.zip(Seq.tabulate(variables.size)(variables.size - _.toFloat)).toMap),
          4
        ) == new Genotype(
          genes = variables.zip(Vector(0f, 1f, 2f, 3f, 2f, 1f)).toMap
        )
    )
  }
}
