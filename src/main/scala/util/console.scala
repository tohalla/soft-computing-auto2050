package util

import scala.io.StdIn
import scala.util.Try

object console {
  def getInt(range: Range = null): Int = {
    println("Syötä kokonaisluku" + (if (range == null) "" else s" väliltä ${range.start} - ${range.end}"))
    val input = Try(StdIn.readInt()).getOrElse(getInt(range))
    if (range == null || range.contains(input))
    input
    else {
      getInt(range)
    }
  }

  def getFloat(range: Range = null): Float = {
    println("Syötä liukuluku" + (if (range == null) "" else s" väliltä ${range.start} - ${range.end}"))
    val input = Try(StdIn.readFloat()).getOrElse(getFloat(range))
    if (range == null || (range.start <= input && range.end >= input))
      input
    else {
      getFloat(range)
    }
  }
}
