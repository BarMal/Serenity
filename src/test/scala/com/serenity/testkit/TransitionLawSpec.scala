package com.serenity.testkit

import cats.Eq
import cats.data.Chain
import cats.laws.discipline.MonadTests
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, ReducerResult, Transition, toTransition}
import com.serenity.testkit.Generators.given
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/** The `Monad` is inherited, which is worth proving rather than assuming: it only exists if the effect log is a lawful
  * `Monoid`. If it were not, reducers migrated in #993 and #994 would compose wrongly.
  */
class TransitionLawSpec extends AnyFunSuite with FunSuiteDiscipline with Configuration with Matchers:

  private val sampleState: AppState = AppState.initial

  private def effect(id: Long): AppEffect = AppEffect.ScheduleCommandRunnerBindingExpiry(id)

  /** `StateT` wraps a function, so there is no structural equality; sampling one state is the standard approach, and
    * bounds how much the law check proves.
    */
  private given transitionEq[A : Eq]: Eq[Transition[A]] =
    Eq.instance { (left, right) =>
      val (leftEffects, (leftState, leftValue))    = left.run(sampleState).run
      val (rightEffects, (rightState, rightValue)) = right.run(sampleState).run
      leftEffects.toList == rightEffects.toList &&
      leftState == rightState &&
      Eq[A].eqv(leftValue, rightValue)
    }

  /** Generates transitions that vary along both axes the type carries: what they log, and how they move the state. A
    * generator that only ever produced pure values would satisfy the laws trivially.
    */
  private given transitionArbitrary[A : Arbitrary]: Arbitrary[Transition[A]] =
    Arbitrary(
      for
        value        <- Arbitrary.arbitrary[A]
        effectCount  <- Gen.choose(0, 3)
        touchesState <- Arbitrary.arbitrary[Boolean]
      yield
        val logged: Transition[Unit] =
          Transition.emitAll(List.tabulate(effectCount)(index => effect(index.toLong)))
        val moved: Transition[Unit] =
          if touchesState then Transition.modify(identity) else Transition.unit
        logged.flatMap(_ => moved).map(_ => value)
    )

  checkAll("Monad[Transition]", MonadTests[Transition].monad[Int, Int, Int])

  test("running a transition collects its state and effects into a ReducerResult") {
    val transition =
      for
        _ <- Transition.emit(effect(1))
        _ <- Transition.modify(identity)
        _ <- Transition.emit(effect(2))
      yield ()

    val result = ReducerResult.fromTransition(sampleState, transition)

    result.state shouldBe sampleState
    result.effects shouldBe List(effect(1), effect(2))
  }

  test("the effect log preserves order across composition") {
    val first  = Transition.emit(effect(1))
    val second = Transition.emit(effect(2))

    ReducerResult.fromTransition(sampleState, first.flatMap(_ => second)).effects shouldBe
      List(effect(1), effect(2))
    ReducerResult.fromTransition(sampleState, second.flatMap(_ => first)).effects shouldBe
      List(effect(2), effect(1))
  }

  test("a ReducerResult round-trips through toTransition unchanged") {
    val original = ReducerResult(sampleState, List(effect(7), effect(8)))

    ReducerResult.fromTransition(AppState.initial, original.toTransition) shouldBe original
  }

  test("a transition with no effects yields the same result as noEffects") {
    ReducerResult.fromTransition(sampleState, Transition.unit) shouldBe ReducerResult.noEffects(sampleState)
  }
