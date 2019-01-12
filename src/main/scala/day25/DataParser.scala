package day25

import scala.io.Source

object DataParser {
  def parse(f: String): List[Point] = {
    Source.fromInputStream(getClass.getResourceAsStream(f))
      .getLines()
      .map(_.trim)
      .map(_.split(","))
      .map(a => Point(a(0).toInt, a(1).toInt, a(2).toInt, a(3).toInt))
      .toList
  }

  def main(args: Array[String]) {
    println(parse("example1.txt"))
  }
}
