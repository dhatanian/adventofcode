package day21

object Data {
  val data = (4,
    """seti 123 0 1
      |bani 1 456 1
      |eqri 1 72 1
      |addr 1 4 4
      |seti 0 0 4
      |seti 0 3 1
      |bori 1 65536 2
      |seti 7902108 7 1
      |bani 2 255 5
      |addr 1 5 1
      |bani 1 16777215 1
      |muli 1 65899 1
      |bani 1 16777215 1
      |gtir 256 2 5
      |addr 5 4 4
      |addi 4 1 4
      |seti 27 0 4
      |seti 0 0 5
      |addi 5 1 3
      |muli 3 256 3
      |gtrr 3 2 3
      |addr 3 4 4
      |addi 4 1 4
      |seti 25 2 4
      |addi 5 1 5
      |seti 17 2 4
      |setr 5 1 2
      |seti 7 2 4
      |eqrr 1 0 5
      |addr 5 4 4
      |seti 5 9 4""".stripMargin)

  /*
  """seti 123 0 1
      |bani 1 456 1
      |eqri 1 72 1
      |addr 1 4 4
      |seti 0 0 4
      |seti 0 3 1
      |bori 1 65536 2     r2 = r1 | 65537
      |seti 7902108 7 1   r1 = 7902108
      |bani 2 255 5       r5 = r2 & 255
      |addr 1 5 1         r1 = r1 + r5
      |bani 1 16777215 1  r1 = r1 & 16777215
      |muli 1 65899 1     r1 = r1 * 65899
      |bani 1 16777215 1  r1 = r1 & 16777215
      |gtir 256 2 5       r5 = 256 > r2
      |addr 5 4 4         r4 = r4 + r5 (0 or 1) if 0: move to addi 5 1 3 else : bani 2 255 5
      |addi 4 1 4
      |seti 27 0 4
      |seti 0 0 5         r5 = 0
      |addi 5 1 3         r3 = r1 + 5
      |muli 3 256 3       r3 = r3 * 256
      |gtrr 3 2 3         r3 = r3 > r2
      |addr 3 4 4         r4 = r4 + r3 (0 or 1) if 0: move to addi 5 1 5 else : return to seti 0 0 5
      |addi 4 1 4         
      |seti 25 2 4        go to setr 5 1 2
      |addi 5 1 5         r5 = r5 + 1
      |seti 17 2 4        go back to addi 5 1 3
      |setr 5 1 2         
      |seti 7 2 4
      |eqrr 1 0 5        r5 = r1 == r0 ==> if r1 == r0 we exit
      |addr 5 4 4        r4 = r4 + r5
      |seti 5 9 4""".stripMargin)
   */
}
