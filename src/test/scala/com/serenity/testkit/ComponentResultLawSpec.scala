package com.serenity.testkit

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.syntax.all.*
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.{Focus, PaneId}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/** The laws are what stop `combine` nesting composites differently depending on association, which would make folding a
  * list of results order-sensitive.
  */
class ComponentResultLawSpec extends AnyFunSuite with FunSuiteDiscipline with Configuration with Matchers:

  private given Eq[ComponentResult] = Eq.fromUniversalEquals

  /** `StateChange` and `ExecuteCommand` are excluded: they carry a function and a command whose separately-constructed
    * instances are never equal, so including them would test the generator rather than the monoid.
    */
  private given Arbitrary[ComponentResult] =
    val leaves: Gen[ComponentResult] =
      Gen.oneOf(
        Gen.const(ComponentResult.NoChange),
        Gen.const(ComponentResult.Dismiss),
        Gen.choose(0, 4).map(id => ComponentResult.FocusTransfer(Focus.EditorPane(PaneId(id))))
      )

    Arbitrary(
      Gen.frequency(
        3 -> leaves,
        1 -> Gen.listOf(leaves).map(ComponentResult.Composite.apply)
      )
    )

  checkAll("Monoid[ComponentResult]", MonoidTests[ComponentResult].monoid)

  test("NoChange is dropped rather than accumulated") {
    val dismiss = ComponentResult.Dismiss

    (ComponentResult.NoChange |+| dismiss) shouldBe dismiss
    (dismiss |+| ComponentResult.NoChange) shouldBe dismiss
    (ComponentResult.NoChange |+| ComponentResult.NoChange) shouldBe ComponentResult.NoChange
  }

  test("combining flattens instead of nesting") {
    val first  = ComponentResult.Dismiss
    val second = ComponentResult.FocusTransfer(Focus.EditorPane(PaneId(1)))
    val third  = ComponentResult.FocusTransfer(Focus.EditorPane(PaneId(2)))

    val flattened = ComponentResult.Composite(List(first, second, third))

    ((first |+| second) |+| third) shouldBe flattened
    (first |+| (second |+| third)) shouldBe flattened
  }

  test("folding a list of results preserves order") {
    val results =
      List(
        ComponentResult.Dismiss,
        ComponentResult.NoChange,
        ComponentResult.FocusTransfer(Focus.EditorPane(PaneId(3)))
      )

    results.combineAll shouldBe
      ComponentResult.Composite(
        List(ComponentResult.Dismiss, ComponentResult.FocusTransfer(Focus.EditorPane(PaneId(3))))
      )
  }
