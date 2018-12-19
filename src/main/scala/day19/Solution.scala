package day19

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

  def executeProcess(program: Array[(Operation, Array[Int])], instructionRegistry: Int, registries: Array[Int]): Array[Int] = {
    val instructionPointer = registries(instructionRegistry)
    if (instructionPointer < 0 || instructionPointer >= program.length)
      return registries
    val line = program(instructionPointer)
    val newRegistries = line._1(line._2, registries)
    newRegistries(instructionRegistry) = newRegistries(instructionRegistry) + 1
    executeProcess(program, instructionRegistry, newRegistries)
  }

  def main(args: Array[String]) {
    val (instructionRegistry, dataString) = Data.data

    val data = dataString
      .split("\n")
      .map(_.trim)
      .map(_.split(" "))
      .map(a => (operations(a(0)), a.tail.map(_.toInt)))

    //Step 1
    //val registries = executeProcess(data, instructionRegistry, Array.fill(6)(0))
    //println(registries.mkString(" "))

    //Step 2
    //val registriesStep2Before = Array.fill(6)(0)
    //registriesStep2Before(0) = 1
    //val registriesStep2After = executeProcess(data, instructionRegistry, registriesStep2Before)
    //println(registriesStep2After.mkString(" "))

    /*
    The registries look like this after each instruction:
      0 0 4 10551374 1 4809752
      0 0 5 10551374 1 4809752
      0 0 7 10551374 1 4809752
      0 0 8 10551374 1 4809753
      0 0 9 10551374 1 4809753
      0 0 10 10551374 1 4809753

    Reading the instructions, clearly something happens when we reach
      0 0 9 10551374 1 10551374
    (there is a comparison between registers 5 and 3 on instruction 9 of the program)

    If we start the program from that step, we see this:
      1 0 4 10551374 2 2370505
      1 0 5 10551374 2 2370505
      1 0 7 10551374 2 2370505
      1 0 8 10551374 2 2370506
      1 0 9 10551374 2 2370506
      1 0 10 10551374 2 2370506
      1 0 2 10551374 2 2370506

    Let's fast forward and see what happens when we reach:
      1 0 9 10551374 10551374 10551373

    Again, register 0 has increased:
      3 0 4 974 248 359
      3 0 5 974 248 359
      3 0 7 974 248 359
      3 0 8 974 248 360


    So registry 0 increases. On line 25 we see:
      addr 2 0 2

    And 2 is our instruction pointer registry. Which means that when r(2) + r(0) >= 36 we break.
    At that line r(2) == 25 and will be incremented by 1 just after.
    So as soon as r(0) >= 10 we break out.

    2 seti 1 5 5 r5 = 1
    3 mulr 4 5 1 r(4) * r(5) -> r(1)
    4 eqrr 1 3 1 r(1) == r(3) -> r(1)
    5 addr 1 2 2 r(1) + r(2) -> r(2)
    6 addi 2 1 2 r(2) + 1 -> r(2)
    7 addr 4 0 0 r(4) + r(0) -> r(0) // we jump that one as long as "eqrr 1 3 1" is not satisfied
    8 addi 5 1 5 r(5) + 1 -> r(5)
    9 gtrr 5 3 1 r(5) > r(3) -> r(1)
   10 addr 2 1 2 r(2) + r(1) -> r(2)
   11 seti 2 6 2   2 -> r(2) // that's how we go back to line 3 - we need to jump that
   12 addi 4 1 4 r(4) + 1 -> r(4)
   13 gtrr 4 3 1 r(4) > r(3) -> r(1)
   14 addr 1 2 2 r(1) + r(2) -> r(2) happens when r4 > r3
   15 seti 1 7 2 r(1) + 7 -> r(2)
   16 mulr 2 2 2 r(2) * r(2) -> r(2)

    We jump it when r(1) > 0 at previous line
    That happens when r(1) == r(3) - at which point we've added r(4) to r(0)

    So we're in a loop between instructions 2 and 16. Rewritten in pseudocode, this looks like:

    r5 = 1
    r1 = r4 * r5
    if(r1 == r3){
      r0 = r0 + r4
    }
    r5 = r5 + 1
    if (r5 > r3) {
      r4 = r4 + 1
      if (r4 > r3) {
        victory
      } else {
        jump back to r5 = 1
      }
    } else {
      jump back to r1 = r4 * r5
    }

    And in Scala (see `Test` class) :

    do{
    r5 = 1
    do {
      r1 = r4 * r5
      if(r1 == r3){
        r0 = r0 + r4
      }
      r5 = r5 + 1
    } while (r5 <= r3)
    r4 = r4 + 1
    }while(r4 <= r3)

    do{
      r4 = r4 + 1
      if (r4 > r3) {
        victory
      } else {
        r5 = r5 + 1
      }
    }while(r4 <= r3)

    ==> The code is computing the sum of the divisors of 10551374 which is 15864120 so we'll use this as the response
     */

    
//    val registriesStep2Before = Array(1, 0, 0, 0, 0, 0)
//    val registriesStep2After = executeProcess(data, instructionRegistry, registriesStep2Before)
  }
}
