package ga

import org.scalatest.FunSuite

class GenoTypeTest extends FunSuite {
  test("should do crossover correctly") {
    assert(
      new Genotype(Vector.tabulate(10)(_.toFloat))
        .crossover(
          new Genotype(Vector.tabulate(10)(10f - _)),
          5
        ) == new Genotype(
          genes = Vector(0f, 1f, 2f, 3f, 4f, 5f, 4f, 3f, 2f, 1f)
        )
    )
  }
}
