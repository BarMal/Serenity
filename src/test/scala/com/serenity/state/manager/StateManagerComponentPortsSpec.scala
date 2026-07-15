package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.state.reducers.ReducerResult
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerComponentPortsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ResizeEventHandler" should "apply the reduced transition before rebalancing through its narrow port" in {
    val program = for
      observed <- Ref.of[IO, List[String]](Nil)
      handler = new ResizeEventHandler(new ResizeEventPort:
        def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
          observed.update(_ :+ s"resize:${result.state.viewportSize}")
        def rebalancePanes(): IO[Unit] =
          observed.update(_ :+ "rebalance"))
      _     <- handler.apply(ResizeEvent(ViewportSize(90, 30)), AppState.initial)
      calls <- observed.get
    yield calls

    program.unsafeRunSync() shouldBe List("resize:Some(ViewportSize(90,30))", "rebalance")
  }

  it should "not rebalance when state application fails" in {
    val program = for
      rebalanced <- Ref.of[IO, Boolean](false)
      handler = new ResizeEventHandler(new ResizeEventPort:
        def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
          IO.raiseError(new IllegalStateException("state failure"))
        def rebalancePanes(): IO[Unit] = rebalanced.set(true))
      result <- handler.apply(ResizeEvent(ViewportSize(90, 30)), AppState.initial).attempt
      value  <- rebalanced.get
    yield (result, value)

    program.unsafeRunSync() match
      case (Left(error), false) => error.getMessage shouldBe "state failure"
      case other                => fail(s"Unexpected resize result: $other")
  }
