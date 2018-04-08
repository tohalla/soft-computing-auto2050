package ga

import org.scalatest.FunSuite

class VariableTest extends FunSuite {
  test("should be fixed if min and max value are same value") {
    assert(new Variable("", "", minValue = 10, maxValue = 10).isFixed)
  }

  test("should scale correctly") {
    assert(new Variable("", "", minValue = 10, maxValue = 20).scaleValue(.5f) == 15)
  }
}
