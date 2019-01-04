package day24

import day24.Data.AttackType._

object Data {

  case class Group(id: Int,
                   units: Int,
                   hpPerUnit: Int,
                   attack: Int,
                   attackType: AttackType,
                   initiative: Int,
                   weaknesses: List[AttackType],
                   immunities: List[AttackType],
                   isImmuneSystem: Boolean) {

    def effectivePower(immuneBoost: Int) = units * (if (isImmuneSystem) immuneBoost + attack else attack)

    def team = if (isImmuneSystem) "Immune System" else "Infection"

    def name = s"$team group $id"

  }

  sealed trait AttackType

  object AttackType {

    object Bludgeoning extends AttackType

    object Radiation extends AttackType

    object Fire extends AttackType

    object Slashing extends AttackType

    object Cold extends AttackType

  }

  val groupRegex = "(\\d+) units each with (\\d+) hit points(\\s*\\(*.*\\)*?) with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)".r

  val example = parse(
    """17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
      |989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3""".stripMargin.split("\n").toList,
    """801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
      |4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".stripMargin.split("\n").toList)

  val data = parse(
    """698 units each with 10286 hit points with an attack that does 133 fire damage at initiative 9
      |6846 units each with 2773 hit points (weak to slashing, cold) with an attack that does 4 slashing damage at initiative 14
      |105 units each with 6988 hit points (weak to bludgeoning; immune to radiation) with an attack that does 616 radiation damage at initiative 17
      |5615 units each with 7914 hit points (weak to bludgeoning) with an attack that does 13 radiation damage at initiative 20
      |1021 units each with 10433 hit points (weak to cold; immune to slashing, bludgeoning) with an attack that does 86 bludgeoning damage at initiative 12
      |6099 units each with 11578 hit points with an attack that does 15 bludgeoning damage at initiative 13
      |82 units each with 1930 hit points (weak to bludgeoning; immune to cold) with an attack that does 179 bludgeoning damage at initiative 5
      |2223 units each with 9442 hit points (immune to bludgeoning) with an attack that does 38 cold damage at initiative 19
      |140 units each with 7594 hit points (weak to radiation) with an attack that does 452 fire damage at initiative 8
      |3057 units each with 3871 hit points (weak to bludgeoning) with an attack that does 11 radiation damage at initiative 16""".stripMargin.split("\n").toList,
    """263 units each with 48098 hit points (immune to radiation; weak to slashing) with an attack that does 293 bludgeoning damage at initiative 2
      |111 units each with 9893 hit points (immune to slashing) with an attack that does 171 fire damage at initiative 18
      |2790 units each with 36205 hit points with an attack that does 25 cold damage at initiative 4
      |3325 units each with 46479 hit points (weak to slashing) with an attack that does 27 radiation damage at initiative 1
      |3593 units each with 6461 hit points (weak to fire, slashing) with an attack that does 3 radiation damage at initiative 15
      |2925 units each with 13553 hit points (weak to cold, bludgeoning; immune to fire) with an attack that does 8 cold damage at initiative 10
      |262 units each with 43260 hit points (weak to cold) with an attack that does 327 radiation damage at initiative 6
      |4228 units each with 24924 hit points (weak to radiation, fire; immune to cold, bludgeoning) with an attack that does 11 cold damage at initiative 11
      |689 units each with 42315 hit points (weak to cold, slashing) with an attack that does 116 fire damage at initiative 7
      |2649 units each with 37977 hit points (weak to radiation) with an attack that does 24 cold damage at initiative 3""".stripMargin.split("\n").toList)


  def toAttackType(s: String): AttackType = s match {
    case "bludgeoning" => Bludgeoning
    case "slashing" => Slashing
    case "fire" => Fire
    case "radiation" => Radiation
    case "cold" => Cold
  }

  def parseWeaknessesOrImmunities(s: String): List[AttackType] = s.replace("weak to ", "").replace("immune to ", "").split(", ").map(toAttackType).toList

  def parseWeaknessesAndImmunities(s: String): (List[AttackType], List[AttackType]) = {
    if (s.isEmpty) {
      return (Nil, Nil)
    }
    val s2 = s.substring(2, s.length - 1)
    val result = s2.split("; ").map(parseWeaknessesOrImmunities).padTo(2, Nil)
    if (s2.startsWith("weak to")) {
      (result(0), result(1))
    } else {
      (result(1), result(0))
    }
  }

  def parseGroup(group: String, isImmuneSystem: Boolean, id: Int): Group = group match {
    case groupRegex(units, hpPerUnit, weaknessesAndImmunities, damage, damageType, initiative) =>
      val (weaknesses, immunities) = parseWeaknessesAndImmunities(weaknessesAndImmunities)
      Group(id + 1, units.toInt, hpPerUnit.toInt, damage.toInt, toAttackType(damageType), initiative.toInt, weaknesses, immunities, isImmuneSystem)
  }

  def parseGroups(groups: List[String], isImmuneSystem: Boolean): List[Group] = groups.zipWithIndex.map(t => parseGroup(t._1, isImmuneSystem, t._2))

  def parse(immuneSystem: List[String], infection: List[String]): (List[Group], List[Group]) = (parseGroups(immuneSystem, true), parseGroups(infection, false))

  def main(args: Array[String]) {
    println(example)
    println(data)
  }

}
