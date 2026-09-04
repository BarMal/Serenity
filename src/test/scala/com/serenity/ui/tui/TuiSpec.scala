package com.serenity.ui.tui

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** Base class for TUI behaviour specs: supplies the effect-system givens a session needs and runs a [[TuiScript]]
  * against a freshly started, fully torn-down session.
  */
abstract class TuiSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given Logger[IO]        = LoggerFactory[IO].getLogger(using LoggerName("TuiSpec"))

  /** A whole scenario's budget. Nothing in a script waits on a timer, so exceeding this means something deadlocked
    * rather than something being slow.
    */
  private val ScenarioTimeout: FiniteDuration = 60.seconds

  export TuiScript.{apply as _, *}

  /** The named keys themselves, so a script reads `press(ArrowDown)`. The builders that would collide with a
    * [[TuiScript]] step of the same name (`ctrl`, `ctrlShift`, `paste`, `text`) stay behind `TuiKeys`.
    */
  export TuiKeys.{
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    Backspace,
    Delete,
    End,
    Enter,
    Escape,
    F1,
    FocusIn,
    FocusOut,
    Home,
    PageDown,
    PageUp,
    ReverseTab,
    Tab,
    modified,
    mouseDrag,
    mouseMove,
    mousePress,
    mouseRelease
  }

  def runTui[A](environment: TuiEnvironment = TuiEnvironment.default)(script: TuiScript[A]): Unit =
    val program = TuiSession.resource(environment).use(session => script.run(session)).void
    program.unsafeRunTimed(ScenarioTimeout).getOrElse(fail(s"TUI scenario did not finish within $ScenarioTimeout"))

  /** The start page (no file argument), the other way a real session begins. */
  def runTuiStartPage[A](script: TuiScript[A]): Unit = runTui(TuiEnvironment.startPage)(script)
end TuiSpec
