package day12

object Solution {

  def parsePot(pot: Char): Boolean = pot match {
    case '#' => true
    case _ => false
  }

  def parseState(initialState: String): List[Boolean] = initialState.toStream.map(parsePot).toList

  def parseTransitions(transitions: String): Map[List[Boolean], Boolean] = transitions.split("\n")
    .map(_.replace("|", "").trim)
    .map(_.split(" => "))
    .map(a => (parseState(a(0)), parsePot(a(1)(0))))
    .filter(_._2)
    .toMap


  //  val initialState: List[Boolean] = parseState(Example.initialState)
  //  val transitions: Map[List[Boolean], Boolean] = parseTransitions(Example.transitions)
  val initialState: List[Boolean] = parseState(Data.initialState)
  val transitions: Map[List[Boolean], Boolean] = parseTransitions(Data.transitions)

  def trim(state: List[Boolean], zeroIndex: Int) = {
    val firstPlantIndex = state.indexOf(true)
    val lastPlantIndex = state.lastIndexOf(true)

    (state.slice(firstPlantIndex, lastPlantIndex + 1), zeroIndex - firstPlantIndex)
  }

  def tick(state: List[Boolean], zeroIndex: Int) = {
    val extraPotsOnLeft = (4 to 1 by -1).map(extraPot => {
      val potState = List.fill(extraPot)(false) ++ state.slice(0, 5 - extraPot)
      transitions.getOrElse(potState, false)
    })

    val extraPotsOnRight = (1 to 4).map(extraPot => {
      val potState = state.slice(state.length - 5 + extraPot, state.length) ++ List.fill(extraPot)(false)
      transitions.getOrElse(potState, false)
    })

    var middlePots = (2 to (state.length - 3)).map(i => state.slice(i - 2, i + 3)).map(transitions.getOrElse(_, false))

    trim((extraPotsOnLeft ++ middlePots ++ extraPotsOnRight).toList, zeroIndex + 2)
  }

  def printState(state: Seq[Boolean]): Unit = println(stateString(state))

  private def stateString(state: Seq[Boolean]) = {
    state.map({
      case true => "#"
      case false => " "
    }).mkString.trim
  }

  def main(args: Array[String]): Unit = {
    println(initialState)
    println(transitions)

    var newState = (initialState, 0)
    //First step
    for (_ <- 1 to 20) {
      newState = (tick _).tupled(newState)
    }


    println(newState._1.zipWithIndex.map({
      case (false, _) => 0
      case (true, i2) => i2 - newState._2
    }).sum)


    //Second step
    newState = (initialState, 0)
    var i = 1L
    while (i < 10000L) {
      val state = (tick _).tupled(newState)
      //      if(state._1 == newState._1){
      //        println(newState._1.zipWithIndex.map({
      //          case (false, _) => 0L
      //          case (true, i2) => i2 - newState._2
      //        }).sum)
      //        System.exit(0)
      //      }
      newState = state
      if (i % 1000 == 0) {
        println(newState._2)
        println(i)
      }
      i += 1
    }

    println(newState._1.zipWithIndex.map({
      case (false, _) => 0L
      case (true, i2) => i2 - 80L + 50000000000L
    }).sum)
  }
}
