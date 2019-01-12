package day25

object ConstellationsCounter {

  def distance(p1: Point, p2: Point): Int =
    math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y) + math.abs(p1.z - p2.z) + math.abs(p1.t - p2.t)

  def constellationsCount(points: List[Point]) = {
    var neighbours = Map.empty[Point, Set[Point]].withDefaultValue(Set.empty)
    for (p1 <- points) {
      for (p2 <- points) {
        if (distance(p1, p2) <= 3) neighbours = neighbours + (p1 -> (neighbours(p1) + p2))
      }
    }

    var constellations = List.empty[Set[Point]]
    for ((p, s) <- neighbours) {
      if (!constellations.flatten.contains(p)) {
        var constellation = s
        var newConstellation = s
        do {
          constellation = newConstellation
          newConstellation = constellation.flatMap(neighbours)
        } while (constellation != newConstellation)

        constellations = newConstellation :: constellations
      }
    }

    constellations.size
  }
}
