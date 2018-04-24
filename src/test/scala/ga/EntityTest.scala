package ga

import org.scalatest.FunSuite

class EntityTest extends FunSuite {
  val variables = Set(
    Variable("1", "", minValue = 0, maxValue = 1),
    Variable("2", "", minValue = 0, maxValue = 10),
    Variable("3", "", minValue = 1, maxValue = 1),
    Variable("4", "", minValue = 0, maxValue = 30),
    Variable("5", "", minValue = 0, maxValue = 30),
    Variable("6", "", minValue = 0, maxValue = 30)
  )

  test("should do crossover correctly") {
    assert(
      Genotype(variables.zip(Seq.tabulate(variables.size)(_.toFloat)).toMap)
        .crossover(
          Genotype(variables.zip(Seq.tabulate(variables.size)(variables.size - _.toFloat)).toMap),
          4
        ) == Genotype(
        genes = variables.zip(Vector(0f, 1f, 2f, 3f, 2f, 1f)).toMap
      )
    )
  }

  test("should do encoding and decoding correctly") {
    val genotype = Genotype.generate(variables).decode.encode;
    assert(genotype.decode.encode == genotype)
    assert(genotype.decode.isInstanceOf[Phenotype])
  }
}
