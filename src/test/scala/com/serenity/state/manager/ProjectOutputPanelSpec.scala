package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.DockedPanelFixtures
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind, ProjectTaskResult, ProjectTaskTerminal}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.PinnedPanelContentReducer
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Project output is an ordinary panel: a running task keeps its latest output in state whether or not the panel is
  * showing, and the panel -- when docked -- follows that output. Hiding the panel neither stops the task nor gets
  * undone by the task's next batch of output.
  */
class ProjectOutputPanelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val command = ProjectTaskCommand(ProjectTaskKind.Build, "make", Path.of("/tmp"), "make", Nil)

  private val running = ProjectTaskTransitions.claimed(AppState.initial, 0L, command)

  private def withOutputPanel(state: AppState, text: String): AppState =
    DockedPanelFixtures.dock(
      state,
      PanelId.ProjectOutput.surfaceId,
      SurfaceContent.Terminal(text, text.length),
      PanelPosition.Bottom,
      14
    )

  private def outputPanelText(state: AppState): Option[String] =
    state.surfaceById(PanelId.ProjectOutput.surfaceId).map(_.content).collect {
      case SurfaceContent.Terminal(text, _) => text
    }

  /** A transition followed by the commit boundary's panel sync, as every commit runs it. */
  private def committed(previous: AppState)(transition: AppState => AppState): AppState =
    PanelContentSync.synced(transition(previous), previous)

  "Task output" should "be kept in state while the output panel is hidden, without showing it" in {
    val next = committed(running)(ProjectTaskTransitions.outputArrived(_, 0L, "compiling").state)

    next.runtime.projectTasks.terminalText shouldBe ProjectTaskTerminal.running(command, "compiling")
    outputPanelText(next) shouldBe None
  }

  it should "reach the output panel when it is docked" in {
    val shown = withOutputPanel(running, ProjectTaskTerminal.started(command))

    val next = committed(shown)(ProjectTaskTransitions.outputArrived(_, 0L, "compiling").state)

    outputPanelText(next) shouldBe Some(ProjectTaskTerminal.running(command, "compiling"))
  }

  "A task's finish" should "not show the output panel again once it has been hidden" in {
    val result = ProjectTaskResult(command, 0, "done")

    val next = committed(running)(ProjectTaskTransitions.finished(_, 0L, Right(result)).state)

    next.runtime.projectTasks.running shouldBe None
    next.runtime.projectTasks.terminalText shouldBe ProjectTaskTerminal.completed(result)
    outputPanelText(next) shouldBe None
  }

  "Showing project output" should "record what it shows as the latest output" in {
    val shown =
      PinnedPanelContentReducer.pinOrUpdateTerminal("Project task cancelled.", PanelPosition.Bottom, 14, running)

    shown.state.runtime.projectTasks.terminalText shouldBe "Project task cancelled."
    outputPanelText(shown.state) shouldBe Some("Project task cancelled.")
  }

  "Pinning project output" should "show the latest output, or say no task has run" in {
    def pinned(state: AppState): AppState =
      PanelTransitions.pinPlan(PanelId.ProjectOutput, PanelPosition.Bottom, state) match
        case PanelPinPlan.Commit(update) => update(state)
        case other                       => fail(s"Expected a commit, got $other")

    outputPanelText(pinned(AppState.initial)) shouldBe Some(ProjectTaskTransitions.NoOutputYet)

    val withOutput = ProjectTaskTransitions.outputArrived(running, 0L, "compiling").state
    outputPanelText(pinned(withOutput)) shouldBe Some(ProjectTaskTerminal.running(command, "compiling"))
  }
