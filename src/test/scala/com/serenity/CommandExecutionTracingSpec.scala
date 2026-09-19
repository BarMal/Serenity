package com.serenity

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

/** Command -> effect observability (issue: palette- and mouse-driven commands executed with no `[COMMAND]` log line
  * while reducer/keybinding-driven ones did, because logging lived in `interpretCommandEffect` rather than the shared
  * `interpretCommand` chokepoint both paths call). Every command execution must emit exactly one enriched `[COMMAND]`
  * line naming the command and its intent, regardless of entry path.
  */
class CommandExecutionTracingSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  /** Records every `.info` message; other levels and the structured (context-carrying) variants are no-ops. Enough to
    * assert what a command execution emits, without pulling in a log4cats testing dependency.
    */
  private class RecordingLogger(ref: Ref[IO, List[String]]) extends SelfAwareStructuredLogger[IO]:
    def info(message: => String): IO[Unit]                                         = ref.update(_ :+ message)
    def info(t: Throwable)(message: => String): IO[Unit]                           = ref.update(_ :+ message)
    def info(ctx: Map[String, String])(message: => String): IO[Unit]               = ref.update(_ :+ message)
    def info(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = ref.update(_ :+ message)

    def debug(message: => String): IO[Unit]                                         = IO.unit
    def debug(t: Throwable)(message: => String): IO[Unit]                           = IO.unit
    def debug(ctx: Map[String, String])(message: => String): IO[Unit]               = IO.unit
    def debug(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = IO.unit
    def error(message: => String): IO[Unit]                                         = IO.unit
    def error(t: Throwable)(message: => String): IO[Unit]                           = IO.unit
    def error(ctx: Map[String, String])(message: => String): IO[Unit]               = IO.unit
    def error(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = IO.unit
    def warn(message: => String): IO[Unit]                                          = IO.unit
    def warn(t: Throwable)(message: => String): IO[Unit]                            = IO.unit
    def warn(ctx: Map[String, String])(message: => String): IO[Unit]                = IO.unit
    def warn(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def trace(message: => String): IO[Unit]                                         = IO.unit
    def trace(t: Throwable)(message: => String): IO[Unit]                           = IO.unit
    def trace(ctx: Map[String, String])(message: => String): IO[Unit]               = IO.unit
    def trace(ctx: Map[String, String], t: Throwable)(message: => String): IO[Unit] = IO.unit

    def isTraceEnabled: IO[Boolean] = IO.pure(false)
    def isDebugEnabled: IO[Boolean] = IO.pure(false)
    def isInfoEnabled: IO[Boolean]  = IO.pure(true)
    def isWarnEnabled: IO[Boolean]  = IO.pure(false)
    def isErrorEnabled: IO[Boolean] = IO.pure(false)

  private class RecordingLoggerFactory(logger: SelfAwareStructuredLogger[IO]) extends LoggerFactory[IO]:
    def getLoggerFromName(name: String): SelfAwareStructuredLogger[IO] = logger
    def fromName(name: String): IO[SelfAwareStructuredLogger[IO]]      = IO.pure(logger)

  private def stateManagerRecording(): (StateManager, Ref[IO, List[String]]) =
    val recorded            = Ref.unsafe[IO, List[String]](Nil)
    val logger              = RecordingLogger(recorded)
    given LoggerFactory[IO] = RecordingLoggerFactory(logger)
    (StateManager.apply(logger).unsafeRunSync(), recorded)

  private def commandLines(recorded: Ref[IO, List[String]]): List[String] =
    recorded.get.unsafeRunSync().filter(_.startsWith("[COMMAND]"))

  "Command execution tracing" should "log a [COMMAND] line naming command and intent for the mouse/effect path" in {
    val (stateManager, recorded) = stateManagerRecording()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "toggle-line-numbers",
          "Toggle line numbers display on/off",
          CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleLineNumbers)),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    val lines = commandLines(recorded)
    lines.size shouldBe 1
    lines.head should include("command=toggle-line-numbers")
    lines.head should include("intent=Settings(TextDisplay(ToggleLineNumbers))")
  }

  it should "log a [COMMAND] line for a command executed through the command palette" in {
    val (stateManager, recorded) = stateManagerRecording()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "toggle-line-numbers".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some("toggle-line-numbers")
    stateManager.applyEvent(Enter).unsafeRunSync()

    val lines = commandLines(recorded)
    lines.count(_.contains("command=toggle-line-numbers")) shouldBe 1
    lines.exists(_.contains("intent=Settings(TextDisplay(ToggleLineNumbers))")) shouldBe true
  }

  it should "log each command exactly once, not twice, on the reducer/keybinding effect path" in {
    // The effect path (AppEffect.ExecuteCommand -> interpretCommandEffect -> interpretCommand) must not double-log now
    // that the [COMMAND] line lives in the shared interpretCommand chokepoint.
    val (stateManager, recorded) = stateManagerRecording()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "toggle-line-numbers".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    commandLines(recorded).count(_.contains("command=toggle-line-numbers")) shouldBe 1
  }
