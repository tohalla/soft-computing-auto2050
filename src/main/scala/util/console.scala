package util

import scala.io.StdIn
import scala.util.Try

object console {
  def getInteger(range: Range = null): Int = {
    val input = Try(StdIn.readInt()).getOrElse(getInteger(range))
    if (range == null || range.contains(input))
      input
    else {
      println(s"Valitse kokonaisluku väliltä ${range.start} - ${range.end}")
      getInteger(range)
    }
  }

  def getFloat(range: Range = null): Float = {
    val input = Try(StdIn.readFloat()).getOrElse(getFloat(range))
    if (range == null || range.contains(input))
      input
    else {
      println(s"Valitse liukuluku väliltä ${range.start} - ${range.end}")
      getFloat(range)
    }
  }
}
