package game.model

import scala.util.Random

case class Game(players: List[Player], activePlayerIndex: Int, dice: Dice, isGameOver: Boolean)

object Game {
  def startWithNames(names: List[String], dice: Dice): Either[String, Game] = {
    val players = names.zipWithIndex.map {
      case (name, i) => Player(id = i, name)
    }

    val (lefts, rights) = players.partitionMap(identity)
    if (lefts.nonEmpty) {
      return Left(lefts.head.message)
    }
    Right(Game(
      rights,
      activePlayerIndex = Random.between(1, names.length),
      Dice.apply(),
      isGameOver = false)
    )
    //    val result = lefts.headOption.toLeft(rights)
    //    result match {
    //      case Left(error) => Left(error.message)
    //      case Right(players) => Right(Game(
    //        players,
    //        activePlayerIndex = scala.util.Random.between(1, names.length),
    //        dice = Dice(),
    //        isGameOver = false
    //      ))
    //    }
  }

  def start(players: List[Player], dice: Dice): Game = {
    Game(players, Random.between(1, players.length), dice, isGameOver = false)
  }
}
