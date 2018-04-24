package ga

import org.scalatest.FunSuite

class VariableTest extends FunSuite {
  val testVariable = Variable("", "", minValue = 10, maxValue = 20)

  test("should be fixed if min and max value are same value") {
    assert(Variable("", "", minValue = 10, maxValue = 10).isFixed)
  }

  test("should scale correctly") {
    assert(testVariable.getScaledValue(.5f) == 15)
    assert(testVariable.getValue(15) == .5f)
  }
}
