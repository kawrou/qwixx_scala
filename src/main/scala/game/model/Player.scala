package game.model

sealed trait PlayerCreationError extends Product with Serializable {
  def message: String = PlayerCreationError.messageFor(this)
}

case object InvalidName extends PlayerCreationError

private object PlayerCreationError {
  def messageFor(error: PlayerCreationError): String = error match {
    case InvalidName => "Player name must be a non-empty string."
  }
}

case class Player(id: Int, name: String, gameCard: GameCard, penalty: Int)

object Player {
  def apply(id: Int, name: String): Either[PlayerCreationError, Player] = {
    if (name.trim.isEmpty) Left(InvalidName)
    else Right(Player(id, name, GameCard.empty, 0))
  }
}