package game.model

import org.scalatest.Inspectors.forAll
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class DiceSpec extends AnyFunSpec with Matchers {
  describe("Dice") {
    it("should create a new default dice") {
      val expectedColors = Set(
        DieColor.White1,
        DieColor.White2,
        DieColor.Red,
        DieColor.Yellow,
        DieColor.Green,
        DieColor.Blue
      )

      val dice = Dice()
      dice.dice.keySet should contain theSameElementsAs expectedColors
      dice.dice.size shouldBe expectedColors.size

      forAll(dice.dice.values) { die =>
        die.value shouldBe 1
        die.disabled shouldBe false
      }

    }

    it("should roll all dice") {
      val dice = Dice()
      val res = dice.rollAll()

      res.dice.keySet shouldBe dice.dice.keySet

      res.dice.foreach { case (color, rolledDie) =>
        val originalDie = dice.dice(color)
        rolledDie.disabled shouldBe originalDie.disabled
        if (!rolledDie.disabled) {
          rolledDie.value should (be >=1 and be <=6)
        }
      }
    }

    it("can disable a non-white dice") {
      val dice = Dice()
      val res = dice.disableDie(DieColor.Red)
      res.dice.foreach {
        case (DieColor.Red, die) => die.disabled shouldBe true
        case (_, die) => die.disabled shouldBe false
      }
    }

    it("can't disable a white die") {
      val dice = Dice()
      val res = dice
        .disableDie(DieColor.White1)
        .disableDie(DieColor.White2)

      res.dice(DieColor.White1).disabled shouldBe false
      res.dice(DieColor.White2).disabled shouldBe false
    }
  }
}
