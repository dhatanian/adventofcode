package day19

object Data {
  val example = (0,
    """seti 5 0 1
      |seti 6 0 2
      |addi 0 1 0
      |addr 1 2 3
      |setr 1 0 0
      |seti 8 0 4
      |seti 9 0 5""".stripMargin)

  val data = (2,
    """addi 2 16 2
      |seti 1 0 4
      |seti 1 5 5
      |mulr 4 5 1
      |eqrr 1 3 1
      |addr 1 2 2
      |addi 2 1 2
      |addr 4 0 0
      |addi 5 1 5
      |gtrr 5 3 1
      |addr 2 1 2
      |seti 2 6 2
      |addi 4 1 4
      |gtrr 4 3 1
      |addr 1 2 2
      |seti 1 7 2
      |mulr 2 2 2
      |addi 3 2 3
      |mulr 3 3 3
      |mulr 2 3 3
      |muli 3 11 3
      |addi 1 6 1
      |mulr 1 2 1
      |addi 1 6 1
      |addr 3 1 3
      |addr 2 0 2
      |seti 0 3 2
      |setr 2 3 1
      |mulr 1 2 1
      |addr 2 1 1
      |mulr 2 1 1
      |muli 1 14 1
      |mulr 1 2 1
      |addr 3 1 3
      |seti 0 9 0
      |seti 0 5 2""".stripMargin)
}
