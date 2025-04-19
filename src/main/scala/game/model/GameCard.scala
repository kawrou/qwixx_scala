package game.model

sealed trait RowColor extends Product with Serializable

object RowColor {
  case object Red extends RowColor

  case object Yellow extends RowColor

  case object Green extends RowColor

  case object Blue extends RowColor

  def isAscending(row: RowColor): Boolean = row match {
    case RowColor.Red | RowColor.Yellow => true
    case RowColor.Green | RowColor.Blue => false
  }
}

sealed trait CardError extends Product with Serializable {
  def message: String = CardError.messageFor(this)
}

private object CardError {
  private def messageFor(error: CardError): String = error match {
    case InvalidNumberOrder => "Number must follow the correct order for this row"
    case NumberAlreadyMarked => "this number has already been marked"
    case NumberOutOfRange => "The number is out of range"
    case UnexpectedError(message) => s"Unexpected Error: $message"
  }
}

case object InvalidNumberOrder extends CardError

case object NumberAlreadyMarked extends CardError

case object NumberOutOfRange extends CardError

case class UnexpectedError(reason: String) extends CardError

case class GameCard private(rows: Map[RowColor, List[Int]]) {
  /**
   * Returns a new [[GameCard]] with the given number marked in the specified row.
   *
   * Fails with a [[CardError]] if the number cannot be placed according to
   * game rules (e.g. marking a number lower than an existing one).
   *
   * @param row the [[RowColor]] row to mark the number in
   * @param number the number to mark
   * @return [[Right]] with the updated GameCard, or [[Left]] with a [[CardError]] describing the issue
   */
  def markNumber(row: RowColor, number: Int): Either[CardError, GameCard] = {
    val currentMarks = rows.getOrElse(row, List.empty)

    if (currentMarks.isEmpty) {
      val updated = rows.updated(row, List(number))
      Right(this.copy(rows = updated))
    }
    else if (RowColor.isAscending(row) && number > currentMarks.last) {
      val updated = rows.updated(row, currentMarks :+ number)
      Right(this.copy(rows = updated))
    }
    else if (!RowColor.isAscending(row) && number < currentMarks.last) {
      val updated = rows.updated(row, currentMarks :+ number)
      Right(this.copy(rows = updated))
    }
    else {
      Left(InvalidNumberOrder)
    }
  }
}

object GameCard {
  // Temporary
  def apply(rows: Map[RowColor, List[Int]]): GameCard = {
    require(rows.keySet == Set(RowColor.Red, RowColor.Yellow, RowColor.Green, RowColor.Blue), "Missing row colors")
    new GameCard(rows)
  }

  /**
   * Returns an empty GameCard with all rows initialized to empty lists.
   *
   * The resulting GameCard contains all [[RowColor]] keys (Red, Yellow, Green, Blue),
   * each mapped to an empty list.
   */
  def empty: GameCard = GameCard(Map(
    RowColor.Red -> List.empty[Int],
    RowColor.Yellow -> List.empty[Int],
    RowColor.Green -> List.empty[Int],
    RowColor.Blue -> List.empty[Int],
  ))

  /**
   * Returns a [[GameCard]] from the given map if it contains exactly all required [[RowColor]] keys.
   *
   * Use this method to safely construct a GameCard from programmatic or saved data.
   * @param rows a map of [[RowColor]] ADT to List of marked numbers.
   * @return Some(GameCard) or 'None' if any row colors are missing.
   */
  def fromMap(rows: Map[RowColor, List[Int]]): Option[GameCard] = {
    if (rows.keySet == Set(RowColor.Red, RowColor.Yellow, RowColor.Green, RowColor.Blue))
      Some(new GameCard(rows))
    else None
  }
}