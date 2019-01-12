package day25

object Part1 {

  def main(args: Array[String]) {
    println(ConstellationsCounter.constellationsCount(DataParser.parse("data.txt")))
  }

}
