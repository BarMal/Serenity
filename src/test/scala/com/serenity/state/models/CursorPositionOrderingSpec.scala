package com.serenity.state.models

import com.serenity.testkit.Generators
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The single line-then-column `Ordering[CursorPosition]` (`#1065`) that replaces the hand-rolled
  * `isAfter`/`isBefore`/`isAtOrBefore` comparisons in `DocumentNavigation` and backs `DirectedRange` (`#1053`).
  */
class CursorPositionOrderingSpec extends AnyFlatSpec with Matchers:

  private val ordering = summon[Ordering[CursorPosition]]

  "Ordering[CursorPosition]" should "order primarily by line" in {
    ordering.lt(CursorPosition(1, 99), CursorPosition(2, 0)) shouldBe true
    ordering.gt(CursorPosition(2, 0), CursorPosition(1, 99)) shouldBe true
  }

  it should "order by column when lines are equal" in {
    ordering.lt(CursorPosition(4, 2), CursorPosition(4, 3)) shouldBe true
    ordering.gt(CursorPosition(4, 3), CursorPosition(4, 2)) shouldBe true
  }

  it should "treat equal line and column as equal" in {
    ordering.equiv(CursorPosition(4, 2), CursorPosition(4, 2)) shouldBe true
    ordering.compare(CursorPosition(4, 2), CursorPosition(4, 2)) shouldBe 0
  }

  it should "consider an atOrBefore comparison inclusive of equality" in {
    val position = CursorPosition(5, 5)
    ordering.lteq(position, position) shouldBe true
  }

class CursorPositionOrderingPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val ordering = summon[Ordering[CursorPosition]]

  property("comparison is antisymmetric") {
    forAll(Generators.genCursorPosition, Generators.genCursorPosition) { (a, b) =>
      if ordering.lt(a, b) then ordering.gt(b, a) shouldBe true
    }
  }

  property("comparison is transitive") {
    forAll(Generators.genCursorPosition, Generators.genCursorPosition, Generators.genCursorPosition) { (a, b, c) =>
      if ordering.lteq(a, b) && ordering.lteq(b, c) then ordering.lteq(a, c) shouldBe true
    }
  }

  property("comparison is total") {
    forAll(Generators.genCursorPosition, Generators.genCursorPosition) { (a, b) =>
      (ordering.lteq(a, b) || ordering.lteq(b, a)) shouldBe true
    }
  }

  property("agrees with the line-then-column tuple ordering") {
    forAll(Generators.genCursorPosition, Generators.genCursorPosition) { (a, b) =>
      ordering.compare(a, b).sign shouldBe Ordering[(Int, Int)].compare((a.line, a.column), (b.line, b.column)).sign
    }
  }
