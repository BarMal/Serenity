package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.std.Supervisor
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.config.{AppConfig, StatusLinePlacement}
import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Damage, TypingActivity}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The typing quiet window ends on its own timer, not on a render-loop animation tick: after the last typed character
  * the window runs out and the expiry commit fires exactly once, however many keystrokes extended it on the way.
  */
class TypingQuietTimerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def typing(untilNanos: Long): AppState =
    val state = AppState.initial
    state.copy(runtime = state.runtime.copy(typingActivity = TypingActivity(Some(untilNanos))))

  private val settled: AppState = AppState.initial

  private def withTimer[A](use: (TypingQuietTimer, Ref[IO, Int]) => IO[A]): A =
    runVirtual(
      Supervisor[IO](await = false).use { supervisor =>
        for
          expiries <- Ref.of[IO, Int](0)
          timer    <- TypingQuietTimer.create(supervisor, expiries.update(_ + 1))
          result   <- use(timer, expiries)
        yield result
      }
    )

  private def commitTyping(timer: TypingQuietTimer, before: AppState): IO[AppState] =
    IO.monotonic.flatMap { now =>
      val after = typing(now.toNanos + TypingActivity.QuietWindow.toNanos)
      timer.onCommit(before, after).as(after)
    }

  "TypingQuietTimer" should "expire typing activity once the quiet window runs out" in {
    val expiries = withTimer { (timer, expiries) =>
      for
        _      <- commitTyping(timer, settled)
        before <- IO.sleep(TypingActivity.QuietWindow - 1.milli) >> expiries.get
        after  <- IO.sleep(2.millis) >> expiries.get
      yield (before, after)
    }

    expiries shouldBe (0, 1)
  }

  it should "push expiry back when another character is typed inside the window" in {
    val expiries = withTimer { (timer, expiries) =>
      for
        first    <- commitTyping(timer, settled)
        _        <- IO.sleep(300.millis)
        _        <- commitTyping(timer, first)
        atFirst  <- IO.sleep(250.millis) >> expiries.get
        atSecond <- IO.sleep(300.millis) >> expiries.get
      yield (atFirst, atSecond)
    }

    expiries shouldBe (0, 1)
  }

  it should "stay idle for commits that do not change typing activity" in {
    val expiries =
      withTimer((timer, expiries) => timer.onCommit(settled, settled) >> IO.sleep(2.seconds) >> expiries.get)

    expiries shouldBe 0
  }

  it should "not arm again when typing activity ends" in {
    val expiries =
      withTimer((timer, expiries) => timer.onCommit(typing(1L), settled) >> IO.sleep(2.seconds) >> expiries.get)

    expiries shouldBe 0
  }

  it should "end a real typing burst through the state manager once the window has passed" in {
    val burstEnded =
      Supervisor[IO](await = false)
        .use { supervisor =>
          for
            sm <- StateManager(
              LoggerFactory[IO].getLogger(using LoggerName("TypingQuietTimerSpec")),
              initialConfig = AppConfig.default
            )
            timer  <- TypingQuietTimer.create(supervisor, TypingQuietTimer.expireIn(sm))
            _      <- sm.runtimeLifecycle.observeCommits(timer.onCommit(_, _))
            _      <- sm.applyEvent(InsertChar('a'))
            during <- sm.getCurrentState.map(_.runtime.typingActivity.isActive)
            _      <- IO.sleep(TypingActivity.QuietWindow + 500.millis)
            after  <- sm.getCurrentState.map(_.runtime.typingActivity.isActive)
          yield (during, after)
        }
        .unsafeRunSync()

    burstEnded shouldBe (true, false)
  }

  private val floatingStatusLine: AppState =
    val state = AppState.initial
    state.copy(persisted =
      state.persisted.copy(config = state.persisted.config.withStatusLinePlacement(StatusLinePlacement.Floating))
    )

  it should "repaint the floating status row when the quiet window expires" in {
    val typingNow =
      floatingStatusLine.copy(runtime = floatingStatusLine.runtime.copy(typingActivity = TypingActivity(Some(1L))))
    val expired = typingNow.copy(runtime = typingNow.runtime.copy(typingActivity = TypingActivity.idle))
    val damage =
      Ref
        .of[IO, Damage](Damage.Nothing)
        .flatMap(seen => AppRuntime.wakeRenderLoopOnCommit(d => seen.set(d))(typingNow, expired) >> seen.get)
        .unsafeRunSync()

    damage should not be Damage.Nothing
  }

  it should "emit that repaint damage when a real typing burst expires through the state manager" in {
    val emitted =
      Supervisor[IO](await = false)
        .use { supervisor =>
          for
            sm <- StateManager(
              LoggerFactory[IO].getLogger(using LoggerName("TypingQuietTimerSpec")),
              initialConfig = floatingStatusLine.persisted.config
            )
            seen  <- Ref.of[IO, Vector[Damage]](Vector.empty)
            timer <- TypingQuietTimer.create(supervisor, TypingQuietTimer.expireIn(sm))
            wake = AppRuntime.wakeRenderLoopOnCommit(damage => seen.update(_ :+ damage))
            _ <- sm.runtimeLifecycle
              .observeCommits((before, after) => wake(before, after) >> timer.onCommit(before, after))
            _      <- sm.applyEvent(InsertChar('a'))
            during <- seen.get.map(_.size)
            _      <- IO.sleep(TypingActivity.QuietWindow + 500.millis)
            after  <- seen.get.map(_.size)
            hidden <- sm.getCurrentState.map(_.floatingStatusLineSurface.isEmpty)
          yield (during, after, hidden)
        }
        .unsafeRunSync()

    val (during, after, _) = emitted
    after should be > during
  }
