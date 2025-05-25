package game.model

import scala.util.Random

sealed trait GameCreationError extends Product with Serializable {
  def message: String = GameCreationError.messageFor(this)
}

private object GameCreationError {
  case object NotEnoughPlayers extends GameCreationError

  case object TooManyPlayers extends GameCreationError

  case class PlayerError(err: PlayerCreationError) extends GameCreationError

  def messageFor(error: GameCreationError): String = error match {
    case NotEnoughPlayers => "There should be a minimum of 2 players"
    case TooManyPlayers => "There should be a maximum of 5 players"
    case PlayerError(err) => s"An error during creating a player ${err.message}"
  }
}

case class Game(
                 players: List[Player],
                 activePlayerIndex: Int,
                 dice: Dice,
                 isGameOver: Boolean = false,
                 hasRolledDice: Boolean = false
               )

object Game {
  def startWithNames(names: List[String]): Either[GameCreationError, Game] = {
    if (names.length < 2) return Left(GameCreationError.NotEnoughPlayers)
    else if (names.length > 5) return Left(GameCreationError.TooManyPlayers)

    val players = names.zipWithIndex.map {
      case (name, i) => Player(id = i, name)
    }

    val (lefts, rights) = players.partitionMap(identity)
    if (lefts.nonEmpty) {
      return Left(GameCreationError.PlayerError(lefts.head))
    }
    Right(Game(
      rights,
      activePlayerIndex = Random.between(1, names.length),
      Dice.apply(),
    )
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
    Game(players, Random.between(1, players.length), dice, isGameOver = false, hasDiceRolled = false)
  }
}
