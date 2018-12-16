package day16

import scala.io.Source

object InstructionExample {


  def parse(strings: List[String]): List[InstructionExample] = strings
    .filterNot(_.trim.isEmpty)
    .map(_.replace("Before: [", ""))
    .map(_.replace("After:  [", ""))
    .map(_.replace("]", ""))
    .grouped(3)
    .map(l => (
      l.head.split(", ").map(Integer.parseInt).array,
      l(1).split(" ").map(Integer.parseInt).array,
      l(2).split(", ").map(Integer.parseInt).array
    ))
    .map(x => InstructionExample(x._1, x._2, x._3))
    .toList

  val example = parse(
    """Before: [3, 2, 1, 1]
      |9 2 1 2
      |After:  [3, 2, 2, 1]""".stripMargin.split("\n").toList)

  val data = parse(Source.fromInputStream(getClass.getResourceAsStream("opcodeexamples.txt")).getLines().toList)

  def main(args: Array[String]) {
    println(data)
    println(example)
  }
}

case class InstructionExample(before: Array[Int], instruction: Array[Int], after: Array[Int]) {
  override def toString: String =
    raw"""Before: ${before.mkString(", ")}
         |${instruction.mkString(", ")}
         |After:  ${after.mkString(", ")}""".stripMargin
}
