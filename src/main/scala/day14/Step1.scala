package day14

object Step1 {

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

  var targetNumberOfRecipes = 306281

  def main(args: Array[String]): Unit = {
    val firstRecipe = new Recipe(EmptyNode, EmptyNode, 3)
    val secondRecipe = new Recipe(firstRecipe, EmptyNode, 7)
    firstRecipe.next = secondRecipe

    var firstElf = firstRecipe
    var secondElf = secondRecipe
    var lastRecipe = secondRecipe

    var constructedRecipes = 2
    while (constructedRecipes < 10 + targetNumberOfRecipes) {
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

    var recipe = firstRecipe
    for (_ <- 1 until targetNumberOfRecipes) {
      recipe = recipe.next.asInstanceOf[Recipe]
    }
    for (_ <- 1 to 10) {
      recipe = recipe.next.asInstanceOf[Recipe]
      print(recipe.score)
    }
    println()
  }
}
