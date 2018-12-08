package day7

object Solution {

      val rules = parse(Data.data)
      val stepTime = 60
      val workers = 5
//  val rules: List[(String, String)] = parse(
//    """Step C must be finished before step A can begin.
//      |Step C must be finished before step F can begin.
//      |Step A must be finished before step B can begin.
//      |Step A must be finished before step D can begin.
//      |Step B must be finished before step E can begin.
//      |Step D must be finished before step E can begin.
//      |Step F must be finished before step E can begin.""")
//  val stepTime = 0
//  val workers = 2

  val ALPHABET = "abcdefghijklmnopqrstuvwxyz".toUpperCase

  def main(args: Array[String]): Unit = {
    val steps = findSteps(rules)
    val letters = rules.flatMap(t => Stream(t._1, t._2)).distinct

    val stepsOrder = steps ++ letters.filterNot(steps.contains(_))
    println(stepsOrder.mkString(""))

    //step 2
    val total = processSteps(stepsOrder, rules, Map.empty, 0)
    println(total)
  }

  def progressWork(steps: Map[String, Int]): Map[String, Int] = steps.mapValues(_ - 1).filterNot(_._2 == 0)

  def durationOfStep(step: String): Int = stepTime + ALPHABET.indexOf(step) + 1

  def canBeStarted(step: String, rules: List[(String, String)], otherStepsWaiting: List[String]): Boolean = {
    !rules.exists(r => otherStepsWaiting.exists(_ == r._1 && step == r._2))
  }

  def processSteps(nextSteps: List[String], rules: List[(String, String)], ongoingWork: Map[String, Int], secondsElapsed: Int): Int = {
    if (nextSteps.isEmpty && ongoingWork.isEmpty) {
      return secondsElapsed - 1
    }

    var newOngoingWork = progressWork(ongoingWork)
    val availableWorkers = workers - newOngoingWork.size

    var availableSteps = nextSteps
    var worker = 1
    var jumpedSteps = List[String]()
    while (availableSteps.nonEmpty && worker <= availableWorkers) {
      val nextStep = availableSteps.head
      availableSteps = availableSteps.tail
      if (canBeStarted(nextStep, rules, jumpedSteps ++ newOngoingWork.keys.toList)) {
        worker += 1
        newOngoingWork = newOngoingWork + (nextStep -> durationOfStep(nextStep))
      } else {
        jumpedSteps = nextStep :: jumpedSteps
      }
    }

    processSteps(jumpedSteps.reverse ++ availableSteps, rules, newOngoingWork, 1 + secondsElapsed)
  }

  def parse(data: String): List[(String, String)] = data
    .split("\n")
    .map(_.trim)
    .map(_.replace("|", "")
      .replaceAll("Step ", "")
      .replaceAll("must be finished before step ", "")
      .replaceAll(" can begin.", ""))
    .map(_.split(" "))
    .map(s => (s(0).trim, s(1).trim)).toList

  def findSteps(subRules: List[(String, String)]): List[String] = subRules match {
    case List() => List()
    case _ =>
      val nextStep = findFirstStep(subRules)
      nextStep :: findSteps(subRules.filterNot(_._2 == nextStep).filterNot(_._1 == nextStep))
  }

  def hasAStepBefore(l: String, rules: List[(String, String)]): Boolean = {
    rules.exists(r => r._2 == l)
  }


  def findFirstStep(rules: List[(String, String)]): String = {
    val letters = rules.flatMap(t => Stream(t._1, t._2)).distinct

    letters.filter(!hasAStepBefore(_, rules)).min
  }
}
