package day15.dijkstra

import de.ummels.dijkstra.Graph

import scala.collection.mutable

/** Implementation of Dijkstra\s algorithm using mutable maps. */
object DijkstraMutableWithLists {
  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, List[N]]) = {
    val active = mutable.Set(source)
    val res = mutable.Map(source -> 0)
    val pred = mutable.Map.empty[N, mutable.Set[N]].withDefault(_ => mutable.Set.empty[N])
    while (active.nonEmpty) {
      val node = active.minBy(res)
      active -= node
      val cost = res(node)
      for ((n, c) <- g(node)) {
        val cost1 = cost + c
        if (cost1 <= res.getOrElse(n, Int.MaxValue)) {
          active += n
          res += (n -> cost1)
          val predecessors = pred(n)
          predecessors += node
          pred.put(n, predecessors)
        }
      }
    }
    (res.toMap, pred.mapValues(_.toList).toMap)
  }

  override def toString = "DijkstraMutable"
}
