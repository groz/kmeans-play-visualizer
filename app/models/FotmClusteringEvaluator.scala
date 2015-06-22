package models

import scala.collection.immutable.IndexedSeq
import scala.util.Random

case class CharacterInfo(
  rating: Int,
  ratingChange: Int,
  weeklyWins: Int,
  weeklyLosses: Int,
  seasonWins: Int,
  seasonLosses: Int)

case class Team(members: Seq[CharacterInfo]) {
  def rating = (members.map(_.rating).sum / members.length.toDouble).toInt
}

object FotmClusteringEvaluator extends App {

  type LadderSnapshot = Seq[CharacterInfo]

  val ladderSize = 10
  val teamSize = 3
  val matchesPerTurn = 2

  val rng = new Random()

  def genPlayer = CharacterInfo(rng.nextInt(), 0, 0, 0, 0, 0)

  def genLadder: LadderSnapshot = (0 until ladderSize).map(i => genPlayer)

  def calc = {
    val ladder: LadderSnapshot = genLadder

    val teams: Seq[Team] = {
      val result = ladder.sliding(teamSize, teamSize).map(Team).toSeq
      if (result.last.members.length == teamSize) result else result.init
    }

    val memberships: Map[CharacterInfo, Team] = ( for {
      t <- teams
      c <- t
    } yield (c, t) ).toMap
  }

  def play(teams: Seq[Team], matchesPerTurn: Int): LadderSnapshot = {
    null
  }

  genLadder.foreach(println)
}
