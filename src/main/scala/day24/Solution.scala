package day24

import day24.Data.Group

object Solution {

  private val (immuneSystem, infection) = Data.data
  private val originalGroups = immuneSystem ++ infection


  private def hasUnits(groups: List[Data.Group]): Boolean = !groups.forall(_.units == 0)


  def fight(immuneBoost: Int) = {

    def damage(attacker: Group, target: Group): Int = {
      val result = if (target.immunities.contains(attacker.attackType)) {
        0
      } else {
        val baseDamageValue = attacker.effectivePower(immuneBoost)
        val actualDamageValue = if (target.weaknesses.contains(attacker.attackType))
          2 * baseDamageValue
        else
          baseDamageValue
        actualDamageValue
      }

      result
    }

    def pickTargetGroup(attacker: Group, possibleTargets: List[Group]): Option[Group] =
      if (possibleTargets.isEmpty) {
        None
      } else {
        val targetsThatCanBeDamaged = possibleTargets.map(target => (target, damage(attacker, target))).filterNot(_._2 == 0)
        if (targetsThatCanBeDamaged.isEmpty) {
          None
        } else {
          Some(targetsThatCanBeDamaged.maxBy(t => (t._2, t._1.effectivePower(immuneBoost), t._1.initiative))._1)
        }
      }


    var groups = originalGroups
    var newGroups: List[Group] = groups
    do {
      groups = newGroups
      //      println(groups.filter(_.isImmuneSystem).map(_.units))
      //      println(groups.filterNot(_.isImmuneSystem).map(_.units))

      var attackedGroupsByTarget = Map.empty[Group, Group]
      groups.sortBy(g => (-g.effectivePower(immuneBoost), -g.initiative)).foreach(g => {
        pickTargetGroup(g, groups.diff(attackedGroupsByTarget.keys.toList).filterNot(g.isImmuneSystem == _.isImmuneSystem)) match {
          case Some(targetGroup) =>
            attackedGroupsByTarget = attackedGroupsByTarget + (targetGroup -> g)
          case _ =>
        }
      })

      val attackedGroupsByAttacker = attackedGroupsByTarget.toStream.groupBy(_._2).mapValues(_.head._1)
      var newGroupsByOldGroups = Map.empty[Group, Group]
      groups.sortBy(-_.initiative).foreach(g => {
        if (attackedGroupsByAttacker.isDefinedAt(g)) {
          val attacker = newGroupsByOldGroups.getOrElse(g, g)
          if (attacker.units > 0) {
            val defender = attackedGroupsByAttacker(g)
            //            println(s"${attacker.name} attacks ${defender.name}")

            val newDefender = defender.copy(units = defender.units - damage(attacker, defender) / defender.hpPerUnit)
            newGroupsByOldGroups = newGroupsByOldGroups + (defender -> newDefender)
          }
        }
      })

      newGroups = groups.diff(newGroupsByOldGroups.keys.toList) ++ newGroupsByOldGroups.values.filterNot(_.units <= 0)
    } while (hasUnits(newGroups.filter(_.isImmuneSystem)) && hasUnits(newGroups.filterNot(_.isImmuneSystem)) && newGroups != groups)
    newGroups
  }

  def main(args: Array[String]) {
    var groups: List[Group] = Nil
    //Part 1
    groups = fight(0)
    println(groups.map(_.units).sum)

    //Part 2

    /*
     * Lower bound 0 is known based on part 1, higher bound was tested manually
     * This dichotomy search reaches lowerBound = 24, higherBound = 36 and then does not
     * terminate for boost = 30. This happens either because units are immune to each other or the
     * remaining fighting units do not inflict enough damage to destroy even one enemy unit
     *
     * So I had to add a loop detection to the fighting logic, then we quickly reach the minimum possible boost.
     */

    var lowerBound = 0
    var higherBound = 1570
    while (higherBound - lowerBound > 1) {
      val boost = (higherBound + lowerBound) / 2
      println(boost)
      groups = fight(boost)
      val remainingImmuneUnits = groups.filter(_.isImmuneSystem).map(_.units).sum
      val remainingInfectionUnits = groups.filterNot(_.isImmuneSystem).map(_.units).sum
      if (remainingImmuneUnits > 0 && remainingInfectionUnits > 0) {
        println("Loop detected, increasing lower bound by 1")
        lowerBound = lowerBound + 1
      } else {
        println(s"$remainingImmuneUnits immune units remaining for boost $boost")
        if (remainingImmuneUnits == 0) {
          lowerBound = boost
        } else {
          higherBound = boost
        }
      }
    }

    println(s"Found result $higherBound with resulting number of immune system units:")
    println(fight(higherBound).filter(_.isImmuneSystem).map(_.units).sum)
  }

}
