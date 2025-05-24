package game.rules

import game.model.{Game, RowColor}

sealed trait PlayerActionError extends Product with Serializable

case object RollDiceError extends PlayerActionError

case object MarkNumberError extends PlayerActionError

case object TakePenaltyError extends PlayerActionError

case object LockRowError extends PlayerActionError

case object SkipTurnError extends PlayerActionError


sealed trait PlayerAction extends Product with Serializable {
  def applyAction(game: Game): Either[PlayerActionError, Game] = PlayerAction.action(game, this)
}

private object PlayerAction {
  private def action(game: Game, action: PlayerAction): Either[PlayerActionError, Game] = action match {
    case RollDice(playerId) =>
      if (game.players(game.activePlayerIndex).id != playerId) Left(RollDiceError)
      else if (!game.hasDiceRolled) Left(RollDiceError)
      else Right(game.copy(dice = game.dice.rollAll()))

    case MarkNumber(playerId, color, number) => {
      val player = game.players.filter(player => player.id == playerId).head
      val playerIdx = game.players.indexOf(player)
      val row = player.gameCard.markNumber(color, number)
      row match {
        case Left(err) => Left(MarkNumberError)
        case Right(gameCard) =>
          Right(game.copy(game.players.updated(playerIdx, player.copy(gameCard = gameCard))))
      }
    }

    case TakePenalty(playerId) => ???
    case LockRow(playerId, color) => ???
    case SkipTurn(playerId) => ???
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
