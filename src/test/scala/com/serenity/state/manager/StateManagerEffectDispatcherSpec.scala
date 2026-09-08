package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.state.models.{BufferId, SurfaceId}
import com.serenity.state.reducers.*
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[CommandEffectInterpreter]] on its own: a pure `AppEffect` match routing to one of its ten dependency
  * closures. Each case is asserted on as "which dependency fired, with which argument" -- nothing else about it is
  * behavior this class owns.
  */
class StateManagerEffectDispatcherSpec extends AnyFlatSpec with Matchers:

  final private class Harness(val calls: Ref[IO, List[String]], val interpreter: CommandEffectInterpreter)

  private def harness(): Harness =
    val calls = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    new Harness(
      calls,
      new CommandEffectInterpreter(
        CommandEffectInterpreter.Dependencies(
          lifecycle = calls.update(_ :+ "lifecycle"),
          command = value => calls.update(_ :+ s"command:$value"),
          theme = value => calls.update(_ :+ s"theme:$value"),
          surface = value => calls.update(_ :+ s"surface:$value"),
          file = value => calls.update(_ :+ s"file:$value"),
          explorer = value => calls.update(_ :+ s"explorer:$value"),
          workflow = value => calls.update(_ :+ s"workflow:$value"),
          lspQueue = value => calls.update(_ :+ s"lspQueue:$value"),
          animation = value => calls.update(_ :+ s"animation:$value"),
          scheduleCommandRunnerBindingExpiry =
            recordedAtMillis => calls.update(_ :+ s"scheduleCommandRunnerBindingExpiry:$recordedAtMillis")
        )
      )
    )

  "CommandEffectInterpreter" should "route CompleteQuit to the lifecycle dependency" in {
    val fixture = harness()

    fixture.interpreter.interpret(AppEffect.CompleteQuit).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List("lifecycle")
  }

  it should "route ExecuteCommand to the command dependency" in {
    val fixture = harness()
    val command = Command.typed(
      "toggle-line-numbers",
      "hint",
      CommandIntent.View(ViewIntent.ToggleTabList),
      CommandCategory.Settings
    )

    fixture.interpreter.interpret(AppEffect.ExecuteCommand(command)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"command:$command")
  }

  it should "route ScheduleCommandRunnerBindingExpiry to its own dependency, carrying the timestamp" in {
    val fixture = harness()

    fixture.interpreter.interpret(AppEffect.ScheduleCommandRunnerBindingExpiry(1234L)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List("scheduleCommandRunnerBindingExpiry:1234")
  }

  it should "route Theme to the theme dependency" in {
    val fixture = harness()
    val effect  = ThemeEffect.SwitchTheme("dracula")

    fixture.interpreter.interpret(AppEffect.Theme(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"theme:$effect")
  }

  it should "route Surface to the surface dependency" in {
    val fixture = harness()
    val effect  = SurfaceEffect.OpenThemePicker

    fixture.interpreter.interpret(AppEffect.Surface(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"surface:$effect")
  }

  it should "route File to the file dependency" in {
    val fixture = harness()
    val effect  = FileEffect.SaveBuffer(BufferId(1))

    fixture.interpreter.interpret(AppEffect.File(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"file:$effect")
  }

  it should "route Explorer to the explorer dependency" in {
    val fixture = harness()
    val effect  = ExplorerEffect.LoadDirectory(PanelPosition.Left, Path.of("/tmp"))

    fixture.interpreter.interpret(AppEffect.Explorer(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"explorer:$effect")
  }

  it should "route Workflow to the workflow dependency" in {
    val fixture = harness()
    val effect  = WorkflowEffect.SubmitFileWorkflow(SurfaceId("modal"))

    fixture.interpreter.interpret(AppEffect.Workflow(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"workflow:$effect")
  }

  it should "route LspQueue to the lspQueue dependency" in {
    val fixture = harness()
    val effect  = LspQueueEffect.DocumentChanged("file:///a.md", com.serenity.lsp.config.LanguageId.Markdown, "hi")

    fixture.interpreter.interpret(AppEffect.LspQueue(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"lspQueue:$effect")
  }

  it should "route Animation to the animation dependency" in {
    val fixture = harness()
    val effect  = AnimationEffect.ClearAll(BufferId(1))

    fixture.interpreter.interpret(AppEffect.Animation(effect)).unsafeRunSync()

    fixture.calls.get.unsafeRunSync() shouldBe List(s"animation:$effect")
  }
