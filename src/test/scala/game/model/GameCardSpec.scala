package game.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class GameCardSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {
  private val emptyRowTestCases = Table(
    ("rowColor", "firstNumber"),
    (RowColor.Red, 5),
    (RowColor.Yellow, 8),
    (RowColor.Green, 12),
    (RowColor.Blue, 9)
  )

  private val ascendingRowTestCases = Table(
    ("rowColor", "firstNumber", "secondNumber"),
    (RowColor.Red, 3, 5),
    (RowColor.Yellow, 6, 8),
  )

  private val descendingRowTestCases = Table(
    ("rowColor", "firstNumber", "secondNumber"),
    (RowColor.Green, 12, 8),
    (RowColor.Blue, 6, 3)
  )

  private val numberOutOfOrderTestCases = Table(
    ("rowColor", "number1", "number2"),
    (RowColor.Red, 5, 3),
    (RowColor.Yellow, 8, 6),
    (RowColor.Green, 6, 9),
    (RowColor.Blue, 1, 5)
  )

  private val rowDirectionTable = Table(
    ("rowColor", "expectedIsAscending"),
    (RowColor.Red, true),
    (RowColor.Yellow, true),
    (RowColor.Green, false),
    (RowColor.Blue, false)
  )

  private val cardErrorTable = Table(
    ("error", "expectedMessage"),
    (InvalidNumberOrder, "Number must follow the correct order for this row"),
    (NumberAlreadyMarked, "this number has already been marked"),
    (NumberOutOfRange, "The number is out of range"),
    (UnexpectedError("unknown error"), "Unexpected Error: unknown error")
  )

  def assertOtherRowsAreEmpty(card: GameCard, exclude: RowColor): Unit = {
    card.rows.view.filterKeys(_ != exclude).values.forall(_.isEmpty) shouldBe true
  }

  describe("GameCard") {
    describe("GameCard.empty") {
      it("should return a card with all empty rows") {
        val card = GameCard.empty
        card.rows.keys.size shouldBe 4
        card.rows.keySet should contain allOf(RowColor.Red, RowColor.Yellow, RowColor.Green, RowColor.Blue)
        card.rows.values.forall(_.isEmpty) shouldBe true
      }
    }

    describe("GameCard.fromMap") {
      it("should return a card initialized from a given map") {
        val card = GameCard.fromMap(
          Map(
            RowColor.Red -> List(5),
            RowColor.Yellow -> List(2),
            RowColor.Green -> List(9),
            RowColor.Blue -> List(10)
          )
        ).getOrElse(fail("Expected Some(GameCard) but got None"))

        card.rows.keySet should contain allOf(RowColor.Red, RowColor.Yellow, RowColor.Green, RowColor.Blue)
        card.rows(RowColor.Red) should contain only 5
        card.rows(RowColor.Yellow) should contain only 2
        card.rows(RowColor.Green) should contain only 9
        card.rows(RowColor.Blue) should contain only 10
      }

      it("should return None if initialized from a map with missing RowColor") {
        GameCard.fromMap(
          Map(
            RowColor.Red -> List(5),
            RowColor.Blue -> List(10)
          )
        ) shouldBe None
      }
    }

    describe("markNumber") {
      it("should return Right(GameCard) when marking a valid number to any empty row") {
        forAll(emptyRowTestCases) { (rowColor, number) =>
          withClue(s"For row: $rowColor, number: $number => ") {
            val card = GameCard.empty
            val result = card
              .markNumber(rowColor, number)
              .getOrElse(fail("Expected Right but got Left"))

            result.rows(rowColor) should contain only number
            assertOtherRowsAreEmpty(result, rowColor)
          }
        }
      }

      it("should add a correct ascending number to (Red / Yellow) rows") {
        forAll(ascendingRowTestCases) { (rowColor, number1, number2) =>
          withClue(s"For row: $rowColor, number1: $number1, number2: $number2 =>") {
            val card = GameCard.empty
              .markNumber(rowColor, number1)
              .getOrElse(fail("Expected Right but got Left"))

            val result = card
              .markNumber(rowColor, number2)
              .getOrElse(fail("Expected Right but got Left"))

            result.rows(rowColor) should contain inOrderOnly(number1, number2)
            assertOtherRowsAreEmpty(result, rowColor)
          }
        }
      }

      it("should add a correct descending number to (Green / Blue) rows") {
        forAll(descendingRowTestCases) { (rowColor, number1, number2) =>
          withClue(s"For row: $rowColor, number1: $number1, number2: $number2 =>") {
            val card = GameCard.empty
              .markNumber(rowColor, number1)
              .getOrElse(fail("Expected Right but got Left"))

            val result = card
              .markNumber(rowColor, number2)
              .getOrElse(fail("Expected Right but got Left"))

            result.rows(rowColor) should contain inOrderOnly(number1, number2)
            assertOtherRowsAreEmpty(result, rowColor)
          }
        }
      }

      it("should return Left(InvalidNumberOrder) when number is out of order") {
        forAll(numberOutOfOrderTestCases) { (rowColor, number1, number2) =>
          withClue(s"For row: $rowColor, number1: $number1, number2: $number2 =>") {

            val card = GameCard.empty
              .markNumber(rowColor, number1)
              .toOption
              .getOrElse(fail("Expected Right but got Left"))

            val result = card.markNumber(rowColor, number2)

            result shouldBe Left(InvalidNumberOrder)
          }
        }
      }
    }

    describe("RowColor") {
      it("should correctly identify if a row is ascending") {
        forAll(rowDirectionTable) { (rowColor, expected) =>
          RowColor.isAscending(rowColor) shouldBe expected
        }
      }
    }
    describe("CardError") {
      it("should return the correct error message for each CardError") {
        forAll(cardErrorTable) { (error, expected) =>
          error.message shouldBe expected
        }
      }
    }
  }
}
