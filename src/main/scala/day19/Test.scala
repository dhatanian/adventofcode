package day19

object Test {

  //Simulation of the loop in Step2 . Does not complete in a timely manner
  def main(args: Array[String]) {
    var r0 = 0L
    var r1 = 0L
    var r3 = 10551374L
    var r4 = 1L
    var r5 = 4809753L

    do {
      print(r0)
      print(" ")
      print(r1)
      print(" ")
      print(r3)
      print(" ")
      print(r4)
      print(" ")
      print(r5)
      println()
      r5 = 1L
      do {
        r1 = r4 * r5
        if (r1 == r3) {
          r0 = r0 + r4
        }
        r5 = r5 + 1L
      } while (r5 <= r3)
      r4 = r4 + 1L
    } while (r4 <= r3)


    println(r0)
    println(r1)
    println(r3)
    println(r4)
    println(r5)
  }

}
