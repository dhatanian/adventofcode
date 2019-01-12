package day23

import scala.io.Source

object Data {

  val nanobotRegex = "pos=<(-?\\d*),(-?\\d*),(-?\\d*)>, r=(\\d*)".r

  case class Nanobot(x: Int, y: Int, z: Int, r: Int)

  val example = parse(
    """pos=<0,0,0>, r=4
      |pos=<1,0,0>, r=1
      |pos=<4,0,0>, r=3
      |pos=<0,2,0>, r=1
      |pos=<0,5,0>, r=3
      |pos=<0,0,3>, r=1
      |pos=<1,1,1>, r=1
      |pos=<1,1,2>, r=1
      |pos=<1,3,1>, r=1""".stripMargin.split("\n").toList)

  val example2 = parse(
    """pos=<10,12,12>, r=2
      |pos=<12,14,12>, r=2
      |pos=<16,12,12>, r=4
      |pos=<14,14,14>, r=6
      |pos=<50,50,50>, r=200
      |pos=<10,10,10>, r=5""".stripMargin.split("\n").toList)

  val data = parse(Source.fromInputStream(getClass.getResourceAsStream("data.txt")).getLines().toList)

  def parse(l: List[String]): List[Nanobot] = l.map({ case nanobotRegex(x, y, z, r) => Nanobot(x.toInt, y.toInt, z.toInt, r.toInt) })

  def main(args: Array[String]) {
    println(data)
    println(example)
  }

}
