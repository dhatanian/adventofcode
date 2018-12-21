package day21

object Solution {

  class Operation(val name: String, val action: (Array[Int], Array[Int]) => Array[Int]) {
    def apply(i: Array[Int], r: Array[Int]): Array[Int] = action(i, r)

    override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Operation] && obj.asInstanceOf[Operation].name == name

    override def hashCode(): Int = name.hashCode

    override def toString: String = name
  }

  object Operation {
    def apply(name: String, subAction: (Array[Int], Array[Int]) => Int): Operation = new Operation(name, subActionToAction(subAction))

    private def subActionToAction(f: (Array[Int], Array[Int]) => Int): (Array[Int], Array[Int]) => Array[Int] = (i: Array[Int], r: Array[Int]) => r.zipWithIndex.map({
      case (_, x) if x == i(2) => f(i, r)
      case x => x._1
    }).array
  }


  def addr: Operation = Operation("addr", (i, r) => r(i(0)) + r(i(1)))

  def addi: Operation = Operation("addi", (i, r) => r(i(0)) + i(1))

  def mulr: Operation = Operation("mulr", (i, r) => r(i(0)) * r(i(1)))

  def muli: Operation = Operation("muli", (i, r) => r(i(0)) * i(1))

  def banr: Operation = Operation("banr", (i, r) => r(i(0)) & r(i(1)))

  def bani: Operation = Operation("bani", (i, r) => r(i(0)) & i(1))

  def borr: Operation = Operation("borr", (i, r) => r(i(0)) | r(i(1)))

  def bori: Operation = Operation("bori", (i, r) => r(i(0)) | i(1))

  def setr: Operation = Operation("setr", (i, r) => r(i(0)))

  def seti: Operation = Operation("seti", (i, r) => i(0))

  def gtir: Operation = Operation("gtir", (i, r) => if (i(0) > r(i(1))) 1 else 0)

  def gtri: Operation = Operation("gtri", (i, r) => if (r(i(0)) > i(1)) 1 else 0)

  def gtrr: Operation = Operation("gtrr", (i, r) => if (r(i(0)) > r(i(1))) 1 else 0)

  def eqir: Operation = Operation("eqir", (i, r) => if (i(0) == r(i(1))) 1 else 0)

  def eqri: Operation = Operation("eqri", (i, r) => if (r(i(0)) == i(1)) 1 else 0)

  def eqrr: Operation = Operation("eqrr", (i, r) => if (r(i(0)) == r(i(1))) 1 else 0)

  def operations = Array(
    addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
  ).map(o => (o.name, o)).toMap

  def executeProcess(program: Array[(Operation, Array[Int])], instructionRegistry: Int, registries: Array[Int], seenValues: Set[Int]): Array[Int] = {
    val instructionPointer = registries(instructionRegistry)
    var newSeenValues = seenValues
    if(instructionPointer == 28){
      if(seenValues(registries(1))){
        sys.exit()
      }
      println(registries(1))
      newSeenValues = seenValues + registries(1)
    }
    if (instructionPointer < 0 || instructionPointer >= program.length)
      return registries
    val line = program(instructionPointer)
    val newRegistries = line._1(line._2, registries)
    newRegistries(instructionRegistry) = newRegistries(instructionRegistry) + 1
    executeProcess(program, instructionRegistry, newRegistries, newSeenValues)
  }

  def main(args: Array[String]) {
    val (instructionRegistry, dataString) = Data.data

    val program = dataString
      .split("\n")
      .map(_.trim)
      .map(_.split(" "))
      .map(a => (operations(a(0)), a.tail.map(_.toInt)))

    /*
      Part 1 : a quick analysis of the program (see Data class) shows that r0 is never written to,
       it's only compared with 1 on one specific line (l28). When both registries are equal, we exit.

       So the question is what is the value of r1 when we first reach line 28, which is fairly easy to answer.
       We find 6483199 for first r1 value and then set it as r0 to exist as quickly as possible
     */
    val registries = Array(0, 0, 0, 0, 0, 0)
    executeProcess(program, instructionRegistry, registries, Set.empty[Int])

    
  }
}
