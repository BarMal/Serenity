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

/** `NoChange` and `Composite` were an identity and a combine written longhand. Now that they are a declared `Monoid`,
  * the laws are what stop `combine` from quietly nesting composites differently depending on association -- which would
  * make folding a list of results order-sensitive in a way no example test would reliably catch.
  */
class ComponentResultLawSpec extends AnyFunSuite with FunSuiteDiscipline with Configuration with Matchers:

  private given Eq[ComponentResult] = Eq.fromUniversalEquals

  /** Drawn from cases with structural equality. `StateChange` and `ExecuteCommand` carry a function and a command
    * respectively, and two separately-constructed functions are never equal, so including them would test the generator
    * rather than the monoid.
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
