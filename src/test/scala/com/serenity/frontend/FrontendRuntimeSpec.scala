package com.serenity.frontend

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.input.{InputHandler, InputRouter}
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Damage}
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1669's remaining scope: `FrontendRuntime` is the bundle `AppRuntime.run` now takes to get its rendering and
  * input behaviour from, in place of the three closures (`makeInputHandler`/`renderFull`/`renderCursorOnly`) it used to
  * receive individually. These are direct unit tests of the bundle itself; `AppRuntime.run` actually dispatching to
  * each of its three members is covered end to end by `AppRuntimeLifecycleSpec`/`AppRuntimeFocusIdleSpec`/
  * `AppRuntimeInputEventSpec`, all of which now build the `FrontendRuntime` they pass it.
  */
class FrontendRuntimeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private class SilentInputHandler extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, com.serenity.keystroke.KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                                        = Stream.never
    override def shutdown: IO[Unit]                                                    = IO.unit

  "FrontendRuntime" should "call through to the input handler it was built with" in {
    val handler = new SilentInputHandler
    val runtime =
      FrontendRuntime(_ => IO.pure(handler), (_, _, _, _, _, _) => IO.unit, (_, _, _, _, _, _) => IO.unit)
    val router = InputRouter
      .create[IO, Event](
        new com.serenity.keystroke.translators.TextEntryTranslator(
          com.serenity.config.AppConfig.default
        )
      )
      .unsafeRunSync()

    runtime.inputHandler(router).unsafeRunSync() shouldBe theSameInstanceAs(handler)
  }

  it should "call through to the renderFull it was built with" in {
    val program = for
      calls <- Ref.of[IO, Int](0)
      runtime = FrontendRuntime(
        _ => IO.pure(new SilentInputHandler),
        (_, _, _, _, _, _) => calls.update(_ + 1),
        (_, _, _, _, _, _) => IO.unit
      )
      _      <- runtime.renderFull(AppState.initial, true, None, Damage.Everything, Map.empty, RenderCaches.create())
      result <- calls.get
    yield result

    program.unsafeRunSync() shouldBe 1
  }

  it should "call through to the renderCursorOnly it was built with, separately from renderFull" in {
    val program = for
      fullCalls   <- Ref.of[IO, Int](0)
      cursorCalls <- Ref.of[IO, Int](0)
      runtime = FrontendRuntime(
        _ => IO.pure(new SilentInputHandler),
        (_, _, _, _, _, _) => fullCalls.update(_ + 1),
        (_, _, _, _, _, _) => cursorCalls.update(_ + 1)
      )
      _ <- runtime.renderCursorOnly(AppState.initial, true, None, Damage.Nothing, Map.empty, RenderCaches.create())
      fullResult   <- fullCalls.get
      cursorResult <- cursorCalls.get
    yield (fullResult, cursorResult)

    program.unsafeRunSync() shouldBe (0, 1)
  }
