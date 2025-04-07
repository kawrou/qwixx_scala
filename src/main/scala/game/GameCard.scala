sealed trait RowColor extends Product with Serializable

object RowColor {
  case object Red extends Rowcolor
  case object Yellow extends RowColor
  case object Green extends Rowcolor
  case object Blue extends RowColor
}

def isAscending(row: Rowcolor) = row match {
  case RowColor.Red | RowColor.Yellow => true
  case RowColor.Green | Rowcolor.Blue => false
}

sealed trait CardError extends Product with Serializable {
  def message: String = CardError.messageFor(this)
}

object CardError {
  protected def messageFor(error: CardError): String = error match {
    case InvalidNumberOrder => "Number must follow the correct order for this row."
    case NumberAlreadyMarked => "this number has already been marked"
    case NumberOutOfRange => "The number is out of range"
    case UnexpectedError(message) => s"Unexpected Error: $message"
  }
}

case object InvalidNumberOrder extends CardError
case object NumberAlreadyMarked extends CardError
case object NumberOutOfRange extends CardError
case object UnexpectedError(override val message: String) extends CardError

case class GameCard private(rows: Map[RowColor, List[Int]])

object GameCard {
  def empty: GameCard = GameCard(Map(
      RowColor.Red -> List.empty[Int],
      RowColor.Yellow -> List.empty[Int],
      RowColor.Green -> List.empty[Int],
      RowColor.Blue -> List.empty[Int],
    ))
}

/** 
 *  A valid number is one that is either larger or smaller than 
 *  the last number in the respective row.
 * */

def markNumber(card: GameCard, row: Rowcolor, number: Int): Either[CardError, GameCard] = {
  val currentRows = card.rows
  val currentMarks = currentRows.getOrElse(row, List.empty)

  if (currentMarks.isEmpty) {
    val updated = currentRows.updated(row, List(number))
    Right(GameCard(updated))
  }
  else if (isAscending(row) && number > currentMarks.last) {
    val updated = currentRows.updated(row, currentMarks :+ number)
    Right(GameCard(updated))
  }
  else if (!isAscending(row) && number < currentMarks.last) {
    val updated = currentRows.updated(row, currentMarks :+ number)
    Right(GameCard(updated))
  }
  else {
    Left(InvalidNumberOrder)
  }
}
