package com.serenity.state.manager

import com.serenity.project.{ProjectTaskCommand, ProjectTaskResult, ProjectTaskRunner, ProjectTaskTerminal}
import com.serenity.state.models.*
import com.serenity.state.reducers.ReducerResult
import com.serenity.ui.layout.PanelPosition

/** The pure state side of project tasks (#1697): which task owns the terminal panel, and how its output and its finish
  * land there. A result for any task but the running one leaves the state untouched.
  */
private[manager] object ProjectTaskTransitions:

  val TerminalPosition: PanelPosition = PanelRegistry.registrationFor(PanelId.ProjectOutput).defaultPosition
  val TerminalSize: Int = PanelRegistry.registrationFor(PanelId.ProjectOutput).defaultSize(TerminalPosition)

  /** What the project output panel shows before any task has run. */
  val NoOutputYet: String = "No project task has run yet."

  /** Records `command` as the running task under `id`, unless a task is already running or `id` is no longer the next
    * one -- another start won the race.
    */
  def claimed(state: AppState, id: Long, command: ProjectTaskCommand): AppState =
    val tasks = state.runtime.projectTasks
    if tasks.running.isDefined || tasks.nextId != id then state
    else withTasks(state, ProjectTasks(id + 1, Some(RunningProjectTask(id, command, ""))))

  def released(state: AppState): AppState =
    if state.runtime.projectTasks.running.isEmpty then state
    else withTasks(state, state.runtime.projectTasks.copy(running = None))

  /** Releases the running task, if any, leaving the terminal to say it was stopped for leaving code mode. */
  def stoppedOnLeavingCode(state: AppState): AppState =
    state.runtime.projectTasks.running.fold(state) { task =>
      withTasks(
        state,
        state.runtime.projectTasks
          .copy(running = None, terminalText = ProjectTaskTerminal.stoppedOnLeavingCode(task.command))
      )
    }

  /** What a session restore keeps: no running task, but the id counter -- ids version task results, so they must never
    * repeat within the process, and a task from before the restore may still post output.
    */
  def acrossRestore(current: AppState): ProjectTasks =
    ProjectTasks(nextId = current.runtime.projectTasks.nextId)

  /** Output and a finish only record the latest text: the project output panel follows it while docked
    * (`PanelContentSync`), but a background result never re-shows a panel that was hidden.
    */
  def outputArrived(state: AppState, id: Long, chunk: String): ReducerResult =
    current(state, id).fold(ReducerResult.noEffects(state)) { task =>
      val updated = task.copy(output = ProjectTaskRunner.appendOutputTail(task.output, chunk))
      ReducerResult.noEffects(
        withTasks(
          state,
          state.runtime.projectTasks
            .copy(running = Some(updated), terminalText = ProjectTaskTerminal.running(task.command, updated.output))
        )
      )
    }

  def finished(state: AppState, id: Long, outcome: Either[Throwable, ProjectTaskResult]): ReducerResult =
    current(state, id).fold(ReducerResult.noEffects(state)) { task =>
      val text         = outcome.fold(ProjectTaskTerminal.failedToStart(task.command, _), ProjectTaskTerminal.completed)
      val afterRelease = released(state)
      ReducerResult.noEffects(withTasks(afterRelease, afterRelease.runtime.projectTasks.copy(terminalText = text)))
    }

  /** The project output panel's content: the latest task output, or a note that no task has run. */
  def terminalContent(state: AppState): SurfaceContent =
    val text = Option(state.runtime.projectTasks.terminalText).filter(_.nonEmpty).getOrElse(NoOutputYet)
    SurfaceContent.Terminal(text, text.length)

  private def current(state: AppState, id: Long): Option[RunningProjectTask] =
    state.runtime.projectTasks.running.filter(_.id == id)

  private def withTasks(state: AppState, tasks: ProjectTasks): AppState =
    state.copy(runtime = state.runtime.copy(projectTasks = tasks))
