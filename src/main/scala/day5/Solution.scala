package day5

import scala.collection.mutable

object Solution {

  def main(args: Array[String]): Unit = {
    val data = Data.data
    //val data = "dabAcCaCBAcCcaDA"
    reactPolymer(data)

    //Second half
    val units = data.map(_.toLower).distinct
    units.foreach(unit => reactPolymer(data.replace(unit.toString, "").replace(unit.toUpper.toString, "")))
  }

  private def reactPolymer(data: String) = {
    val stack = new mutable.ArrayStack[Char]()

    for (c <- data) {
      if (stack.isEmpty) {
        stack += c
      } else {
        val lastChar = stack.pop()
        if (!shouldExplode(lastChar, c)) {
          stack += lastChar
          stack += c
        }
      }
    }

    println(stack.length)
  }

  def shouldExplode(c1: Char, c2: Char): Boolean = {
    c1 != c2 && c1.toLower == c2.toLower
  }
}
