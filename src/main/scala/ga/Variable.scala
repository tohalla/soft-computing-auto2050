package ga

import util.console

case class Variable(text: String, unit: String, minValue: Float, maxValue: Float) {
  val isFixed: Boolean = minValue == maxValue

  def getScaledValue(value: Float): Float =
    if (isFixed) minValue else minValue + value * (maxValue - minValue)

  def getValue(scaledValue: Float): Float =
    if (isFixed) 1f else (scaledValue - minValue) / (maxValue - minValue)

  def updated(minValue: Float, maxValue: Float): Variable = copy(minValue = minValue, maxValue = maxValue)

  def promptManage: Variable = {
    println(
      s"""
        |Muokataan muuttujaa: $this
        |1. Muokkaa ylä- ja alarajaa
        |2. Pakota arvoon
        |3. Valmis
      """.stripMargin
    )
    console.getInt(1, 3) match {
      case 1 => {
        val min = console.getFloat(query = Some("Aseta pienin mahdollinen arvo"))
        copy(
          minValue = min,
          maxValue = console.getFloat(minValue = Some(min), query = Some("Aseta suurin mahdollinen arvo"))
        )
      }
      case 2 => {
        val value = console.getFloat()
        copy(maxValue = value, minValue = value)
      }
      case _ => this
    }
  }

  override def toString: String = s"$text: ${if (minValue == maxValue) minValue else s"$minValue - $maxValue"} $unit"
}
