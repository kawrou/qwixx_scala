package game.model

case class Player(id: Int, name: String, gameCard: GameCard, penalty: Int)

object Player {
  def markNumber(player: Player, rowColor: RowColor, number: Int)= {
    val maybeCard = player.gameCard.markNumber(rowColor, number)
    maybeCard match {
      case Left(err) => ???
      case Right(card) => Player(player.id, player.name, card, player.penalty)
    }
  }
}