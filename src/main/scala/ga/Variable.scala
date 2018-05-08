package ga

import util.console

/**
  * @param text Muuttujan nimi tai kuvaus. Tarkoitettu selventämään muuttujan toimintaa sovelluksen käyttäjälle.
  * @param unit Muuttujan arvon yksikkö. Tarkoitettu selventämään muuttujan toimintaa sovelluksen käyttäjälle.
  * @param minValue Pienin mahdollinen arvo, jonka muuttuja voi saada (olettaen, että kerroin annetaan väliltä [0, 1])
  * @param maxValue Suurin arvo, jonka muuttuja voi saada (olettaen, että kerroin annetaan väliltä [0, 1])
  */
case class Variable(text: String, unit: String, minValue: Float, maxValue: Float) {
  val isFixed: Boolean = minValue == maxValue

  // Skaalaa syötetyn arvon minimi ja maksimi arvon välille
  def getScaledValue(value: Float): Float =
    if (isFixed) minValue else minValue + value * (maxValue - minValue)

  // Laskee skaalatun luvun perusteella alkuperäisen luvun
  def getValue(scaledValue: Float): Float =
    if (isFixed) 1f else (scaledValue - minValue) / (maxValue - minValue)

  def updated(minValue: Float, maxValue: Float): Variable = copy(minValue = minValue, maxValue = maxValue)

  // Kysyy käyttäjää syöttämään uudet ylä- ja ala-rajat muuttujalle. Palauttaa muokatun muuttujan
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
      case 1 =>
        val min = console.getFloat(query = Some("Aseta pienin mahdollinen arvo"))
        copy(
          minValue = min,
          maxValue = console.getFloat(
            minValue = Some(min), // Ylärajan tulee olla suurempi, kuin alarajan
            query = Some("Aseta suurin mahdollinen arvo")
          )
        )
      case 2 => // asetetaan muuttuja vakioksi
        val value = console.getFloat()
        copy(maxValue = value, minValue = value)
      case _ => this
    }
  }

  override def toString: String = s"$text: ${if (minValue == maxValue) minValue else s"$minValue - $maxValue"} $unit"
}
