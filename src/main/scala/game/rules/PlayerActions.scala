package game.rules

import game.model.{Game, Player, RowColor}

import scala.:+

sealed trait PlayerActionError extends Product with Serializable

case object RollDiceError extends PlayerActionError

case object MarkNumberError extends PlayerActionError

case object TakePenaltyError extends PlayerActionError

case object LockRowError extends PlayerActionError

case object SkipTurnError extends PlayerActionError

case object PlayerNotFoundError extends PlayerActionError


sealed trait PlayerAction extends Product with Serializable {
  def applyActionTo(game: Game): Either[PlayerActionError, Game] = PlayerAction.action(game, this)
}

object PlayerAction {
  def action(game: Game, action: PlayerAction): Either[PlayerActionError, Game] = action match {
    case RollDice(playerId) =>
      if (game.players(game.activePlayerIndex).id != playerId)
        Left(RollDiceError)
      else if (game.hasRolledDice)
        Left(RollDiceError)
      else
        Right(game.copy(dice = game.dice.rollAll()))

    case MarkNumber(playerId, color, number) =>
      getPlayer(game, playerId).flatMap { case (player, idx) =>
        player.gameCard
          .markNumber(row = color, number = number)
          .left.map(_ => MarkNumberError)
          .map { updatedGameCard =>
            val updatedPlayer = player.copy(gameCard = updatedGameCard)
            game.copy(players = game.players.updated(idx, updatedPlayer))
          }
      }

    //      for {
    //        result <- getPlayer(game, playerId)
    //        (player, idx) = result
    //        updatedGameCard <- player.gameCard
    //          .markNumber(row = color, number = number)
    //          .left.map(err => MarkNumberError)
    //        updatedPlayer = player.copy(gameCard = updatedGameCard)
    //        updatedGame = game.copy(players = game.players.updated(idx, updatedPlayer))
    //      } yield updatedGame


    case TakePenalty(playerId) =>
      //      getPlayer(game, playerId).flatMap { case (player, idx) =>
      //        val updatedPlayer = player.copy(penalty = player.penalty + 1)
      //        Right(game.copy(players = game.players.updated(idx, updatedPlayer)))
      //      }

      for {
        playerTuple <- getPlayer(game, playerId)
        (player, idx) = playerTuple
        updatedPlayer = player.copy(penalty = player.penalty + 1)
      } yield game.copy(players = game.players.updated(idx, updatedPlayer))

    case LockRow(playerId, color) => ???

    case SkipTurn(playerId) =>
      for {
        playerTuple <- getPlayer(game, playerId)
        (player, idx) = playerTuple
        updatedPlayer = player.copy(finishedTurn = true)
      } yield game.copy(game.players.updated(idx, updatedPlayer))
  }

  private def getPlayer(game: Game, playerId: Int): Either[PlayerActionError, (Player, Int)] = {
    game.players.zipWithIndex.find { case (p, _) => p.id == playerId } match {
      case Some((player, idx)) => Right((player, idx))
      case None => Left(PlayerNotFoundError)
    }
  }
}

//  Roll a dice?
case class RollDice(playerId: Int) extends PlayerAction

//  Mark a number
case class MarkNumber(playerId: Int, color: RowColor, number: Int) extends PlayerAction

//  Take a penalty
case class TakePenalty(playerId: Int) extends PlayerAction

//  Lock a row
case class LockRow(playerId: Int, color: RowColor) extends PlayerAction

//  Skip a turn
case class SkipTurn(playerId: Int) extends PlayerAction