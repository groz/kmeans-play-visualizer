package models

import models.FotmClusteringEvaluator.LadderSnapshot

import scala.collection.immutable.IndexedSeq
import scala.util.Random

case class CharacterId(uid: String)

case class CharacterInfo(
  id: CharacterId,
  rating: Int,
  weeklyWins: Int,
  weeklyLosses: Int,
  seasonWins: Int,
  seasonLosses: Int)

case class Team(members: Seq[CharacterId]) {
  def rating(ladder: LadderSnapshot) = {
    val charInfos: Seq[CharacterInfo] = members.map(ladder)
    (charInfos.map(_.rating).sum / members.size.toDouble).toInt
  }

}

object FotmClusteringEvaluator extends App {

  type LadderSnapshot = Map[CharacterId, CharacterInfo]

  val ladderSize = 10
  val teamSize = 3
  val matchesPerTurn = 2

  lazy val rng = new Random()

  def genPlayer = {
    val id = java.util.UUID.randomUUID().toString
    CharacterInfo(CharacterId(id), 1500, 0, 0, 0, 0)
  }

  def genLadder(nPlayers: Int): LadderSnapshot = (0 until nPlayers).map(i => {
    val player = genPlayer
    (player.id, player)
  }).toMap

  def prepareData = {
    val ladder: LadderSnapshot = genLadder(ladderSize)

    val teams: Seq[Team] = {
      val result = ladder.toSeq.sliding(teamSize, teamSize).map(players => Team(players.map(_._1))).toSeq
      if (result.last.members.length == teamSize) result else result.init
    }

    val memberships: Map[CharacterId, Team] = {
      for {
        t <- teams
        c <- t.members
      } yield (c, t)
    }.toMap

    val shuffledTeams = new Random().shuffle(memberships.values.toSeq)

    val matches = shuffledTeams.sliding(2, 2).take(matchesPerTurn)

    val nextLadder = matches.foldLeft(ladder) { (l, teams) => play(l, teams.head, teams.last) }


  }

  def calcRating(charId: CharacterId, team: Team, won: Boolean)(ladder: LadderSnapshot): Int = {
    val teamRating = team.rating(ladder)
    val charInfo = ladder(charId)
    if (won) {
      charInfo.rating + calcRatingChange(charInfo.rating, teamRating)
    } else {
      charInfo.rating - calcRatingChange(teamRating, charInfo.rating)
    }
  }

  def calcRatingChange(winnerRating: Double, loserRating: Double): Int = {
    val chance = 1.0 / (1.0 + Math.pow(10, (loserRating - winnerRating)/400.0))
    val k = 32
    Math.round(k * (1 - chance)).toInt
  }

  def play(currentLadder: LadderSnapshot, wTeam: Team, lTeam: Team): LadderSnapshot = {
    //val (winners, losers) = if (rng.nextBoolean()) (team1, team2) else (team2, team1)

    val ladderWithWinners: LadderSnapshot =
      wTeam.members.foldLeft(currentLadder) { (ladder, charId) =>
        val ratingDelta = calcRating(charId, lTeam, won = true)(currentLadder)
        val charInfo = currentLadder(charId)

        val newCharInfo = charInfo.copy(
          rating = ratingDelta,
          weeklyWins = charInfo.weeklyWins + 1,
          seasonWins = charInfo.seasonWins + 1)

        ladder.updated(charInfo.id, newCharInfo)
      }

    val result: LadderSnapshot =
      lTeam.members.foldLeft(ladderWithWinners) { (ladder, charId) =>
        val ratingDelta = calcRating(charId, wTeam, won = false)(currentLadder)
        val charInfo = currentLadder(charId)

        val newCharInfo = charInfo.copy(
          rating = ratingDelta,
          weeklyLosses = charInfo.weeklyLosses + 1,
          seasonLosses = charInfo.seasonLosses + 1)

        ladder.updated(charInfo.id, newCharInfo)
      }

    result
  }

  genLadder(ladderSize).foreach(println)
}
