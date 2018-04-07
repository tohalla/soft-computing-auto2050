package util

import scala.io.StdIn
import scala.util.Try

object console {
  def getInt(minValue: Int, maxValue: Int): Int = getInt(Some(minValue), Some(maxValue))

  def getInt(
    minValue: Option[Int] = None,
    maxValue: Option[Int] = None,
    query: Option[String] = Some("Syötä kokonaisluku")
  ): Int = {
    if (query.isDefined)
      print(query +
        (if (minValue.isEmpty && maxValue.isEmpty) "" else s" (${minValue.getOrElse("")} - ${maxValue.getOrElse("")})")
      )
    val input = Try(StdIn.readInt()).getOrElse(getInt(minValue, maxValue))
    if (
      (minValue.isEmpty && maxValue.isEmpty) ||
        (minValue.getOrElse(input) <= input && maxValue.getOrElse(input) >= input)
    )
      input
    else getInt(minValue, maxValue)
  }

  def getFloat(minValue: Float, maxValue: Float): Float = getFloat(Some(minValue), Some(maxValue))

  def getFloat(
    minValue: Option[Float] = None,
    maxValue: Option[Float] = None,
    query: Option[String] = Some("Syötä liukuluku")
  ): Float = {
    if (query.isDefined)
      println(query +
        (if (minValue.isEmpty && maxValue.isEmpty) "" else s" (${minValue.getOrElse("")} - ${maxValue.getOrElse("")})")
      )
    val input = Try(StdIn.readFloat()).getOrElse(getFloat(minValue, maxValue))
    if (
      (minValue.isEmpty && maxValue.isEmpty) ||
        (minValue.getOrElse(input) <= input && maxValue.getOrElse(input) >= input)
    )
      input
    else getFloat(minValue, maxValue)
  }
}
