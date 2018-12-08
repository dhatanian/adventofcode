package day8

object Solution {

  case class Tree(data: List[Int] = Nil,
                  children: List[Tree] = Nil,
                  childCount: Int,
                  metadataCount: Option[Int] = Option.empty)

  val data = Data.data
  //  val data = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"


  def main(args: Array[String]): Unit = {
    val input = data.split(" ").map(Integer.parseInt).toSeq

    val tree = buildTree(input.tail, Tree(childCount = input.head))._2

    println(tree)

    println(sumData(List(tree)))

    //Second part
    println(tree.children.size)
    println(getValue(tree))
  }

  def getValue(tree: Tree): Int = tree match {
    case Tree(data, Nil, _, _) => data.sum
    case Tree(data, children, _, _) => data.map(d => if (d == 0 || d > children.size) 0 else getValue(children(d - 1))).sum
  }

  def sumData(trees: List[Tree]): Int = trees match {
    case t :: rest => t.data.sum + sumData(t.children) + sumData(rest)
    case Nil => 0
  }

  def buildTree(input: Seq[Int], tree: Tree): (Seq[Int], Tree) = {
    tree match {
      case Tree(_, _, _, None) =>
        buildTree(input.tail, tree.copy(metadataCount = Some(input.head)))
      case Tree(_, children, childCount, _) if children.size < childCount =>
        val nextCall = buildTree(input.tail, Tree(childCount = input.head))
        buildTree(nextCall._1, tree.copy(children = tree.children :+ nextCall._2))
      case Tree(metadata, children, childCount, Some(metadataCount)) if children.size == childCount && metadata.size < metadataCount =>
        buildTree(input.tail, tree.copy(data = input.head :: tree.data))
      case Tree(metadata, children, childCount, Some(metadataCount)) if children.size == childCount && metadata.size == metadataCount => {
        (input, tree)
      }
    }
  }
}
