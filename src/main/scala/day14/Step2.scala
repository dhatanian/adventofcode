package day14

object Step2 {

  trait Node

  object EmptyNode extends Node

  class Recipe() extends Node {
    var previous: Node = EmptyNode
    var next: Node = EmptyNode
    var score: Int = 0

    def this(previous: Node, next: Node, score: Int) = {
      this()
      this.previous = previous
      this.next = next
      this.score = score
    }
  }

  object Recipe {
    def unapply(arg: Recipe): Option[(Node, Node, Int)] = Some((arg.previous, arg.next, arg.score))
  }

  var targetRecipes = "306281".map(c => c.toString).map(Integer.parseInt).toList.reverse

  def foundPattern(lastRecipe: Recipe) = {
    var currentRecipe = lastRecipe
    if (lastRecipe.score != targetRecipes(0) && lastRecipe.previous != EmptyNode) {
      currentRecipe = lastRecipe.previous.asInstanceOf[Recipe]
    }
    var i = 0;
    while (i < targetRecipes.length && currentRecipe.previous != EmptyNode && targetRecipes(i) == currentRecipe.score) {
      currentRecipe = currentRecipe.previous.asInstanceOf[Recipe]
      i += 1
    }
    i == targetRecipes.length
  }

  def main(args: Array[String]): Unit = {
    val time = System.currentTimeMillis();
    val firstRecipe = new Recipe(EmptyNode, EmptyNode, 3)
    val secondRecipe = new Recipe(firstRecipe, EmptyNode, 7)
    firstRecipe.next = secondRecipe

    var firstElf = firstRecipe
    var secondElf = secondRecipe
    var lastRecipe = secondRecipe

    var constructedRecipes = 2
    while (!foundPattern(lastRecipe)) {
      val totalScore = firstElf.score + secondElf.score
      if (totalScore >= 10) {
        val firstNewRecipeScore = totalScore / 10
        val secondNewRecipeScore = totalScore % 10
        val firstNewRecipe = new Recipe(lastRecipe, EmptyNode, firstNewRecipeScore)
        lastRecipe.next = firstNewRecipe
        val secondNewRecipe = new Recipe(firstNewRecipe, EmptyNode, secondNewRecipeScore)
        firstNewRecipe.next = secondNewRecipe
        lastRecipe = secondNewRecipe
        constructedRecipes += 2
      } else {
        constructedRecipes += 1
        val newRecipeScore = totalScore
        val newRecipe = new Recipe(lastRecipe, EmptyNode, newRecipeScore)
        lastRecipe.next = newRecipe
        lastRecipe = newRecipe
      }
      val firstElfCurrentScore = firstElf.score
      val secondElfCurrentScore = secondElf.score
      for (_ <- 0 to firstElfCurrentScore) {
        firstElf.next match {
          case EmptyNode => firstElf = firstRecipe
          case Recipe(_, _, _) => firstElf = firstElf.next.asInstanceOf[Recipe]
        }
      }
      for (_ <- 0 to secondElfCurrentScore) {
        secondElf.next match {
          case EmptyNode => secondElf = firstRecipe
          case _ => secondElf = secondElf.next.asInstanceOf[Recipe]
        }
      }
    }

    println(constructedRecipes - targetRecipes.length)
    // Or, depending on whether we built one or two recipes (we can certainly pick programmatically)
    println(constructedRecipes - targetRecipes.length - 1)
    println(System.currentTimeMillis() - time)
  }
}
