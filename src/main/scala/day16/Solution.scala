package day16

import java.util

import scala.io.Source

object Solution {

  class Operation(val name: String, val action: (Array[Int], Array[Int]) => Array[Int]) {
    def apply(i: Array[Int], r: Array[Int]): Array[Int] = action(i, r)

    override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Operation] && obj.asInstanceOf[Operation].name == name

    override def hashCode(): Int = name.hashCode

    override def toString: String = name
  }

  object Operation {
    def apply(name: String, subAction: (Array[Int], Array[Int]) => Int): Operation = new Operation(name, subActionToAction(subAction))
  }

  def subActionToAction(f: (Array[Int], Array[Int]) => Int): (Array[Int], Array[Int]) => Array[Int] = (i: Array[Int], r: Array[Int]) => r.zipWithIndex.map({
    case (_, x) if x == i(3) => f(i, r)
    case x => x._1
  }).array

  def addr: Operation = Operation("addr", (i, r) => r(i(1)) + r(i(2)))

  def addi: Operation = Operation("addi", (i, r) => r(i(1)) + i(2))

  def mulr: Operation = Operation("mulr", (i, r) => r(i(1)) * r(i(2)))

  def muli: Operation = Operation("muli", (i, r) => r(i(1)) * i(2))

  def banr: Operation = Operation("banr", (i, r) => r(i(1)) & r(i(2)))

  def bani: Operation = Operation("bani", (i, r) => r(i(1)) & i(2))

  def borr: Operation = Operation("borr", (i, r) => r(i(1)) | r(i(2)))

  def bori: Operation = Operation("bori", (i, r) => r(i(1)) | i(2))

  def setr: Operation = Operation("setr", (i, r) => r(i(1)))

  def seti: Operation = Operation("seti", (i, r) => i(1))

  def gtir: Operation = Operation("gtir", (i, r) => if (i(1) > r(i(2))) 1 else 0)

  def gtri: Operation = Operation("gtri", (i, r) => if (r(i(1)) > i(2)) 1 else 0)

  def gtrr: Operation = Operation("gtrr", (i, r) => if (r(i(1)) > r(i(2))) 1 else 0)

  def eqir: Operation = Operation("eqir", (i, r) => if (i(1) == r(i(2))) 1 else 0)

  def eqri: Operation = Operation("eqri", (i, r) => if (r(i(1)) == i(2)) 1 else 0)

  def eqrr: Operation = Operation("eqrr", (i, r) => if (r(i(1)) == r(i(2))) 1 else 0)

  def operations = Array(
    addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr

  )

  def operationSatisfies(e: InstructionExample, o: Operation): Boolean = util.Arrays.equals(o(e.instruction, e.before), e.after)

  def validOperations(e: InstructionExample): List[Operation] = operations.filter(operationSatisfies(e, _)).toList

  def commonItems[T](lists: List[List[T]]): List[T] = lists match {
    case head :: Nil => head
    case head :: tail => head intersect commonItems(tail)
  }

  def solvePossibleOperations[T](mappings: Map[Int, List[T]]): List[(Int, T)] =
    if (mappings.isEmpty) Nil else {
      val solvedCombination = mappings.filter(_._2.size == 1).mapValues(_.head).head
      val mappingsWithoutSolvedCombination = mappings
        .filterKeys(_ != solvedCombination._1)
        .mapValues(_.filterNot(item => item == solvedCombination._2))
      solvedCombination :: solvePossibleOperations(mappingsWithoutSolvedCombination)
    }

  def main(args: Array[String]) {
    val examples = InstructionExample.data

    //Step 1
    println(examples.map(validOperations).count(_.size >= 3))

    //Step 2
    val possibleOperationsPerOpcode = examples.groupBy(_.instruction(0)).mapValues(_.map(x => validOperations(x))).mapValues(commonItems)
    val operationsPerOpcode = solvePossibleOperations(possibleOperationsPerOpcode).toMap
    val instructions = Source.fromInputStream(getClass.getResourceAsStream("instructions.txt"))
      .getLines()
      .map(_.trim.split(" ").map(Integer.parseInt))
      .toList

    val finalRegistry = instructions.foldLeft(Array(0, 0, 0, 0))((r, i) => operationsPerOpcode(i(0))(i, r))
    println(finalRegistry.mkString(" "))
  }
}
