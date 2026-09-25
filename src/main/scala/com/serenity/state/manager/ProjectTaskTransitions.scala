package com.serenity.state.manager

import com.serenity.project.{ProjectTaskCommand, ProjectTaskResult, ProjectTaskRunner, ProjectTaskTerminal}
import com.serenity.state.models.*
import com.serenity.state.reducers.{PinnedPanelContentReducer, ReducerResult}
import com.serenity.ui.layout.PanelPosition

/** The pure state side of project tasks (#1697): which task owns the terminal panel, and how its output and its finish
  * land there. A result for any task but the running one leaves the state untouched.
  */
private[manager] object ProjectTaskTransitions:

  val TerminalPosition: PanelPosition = PanelPosition.Bottom
  val TerminalSize: Int               = 14

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

  /** What a session restore keeps: no running task, but the id counter -- ids version task results, so they must never
    * repeat within the process, and a task from before the restore may still post output.
    */
  def acrossRestore(current: AppState): ProjectTasks =
    ProjectTasks(nextId = current.runtime.projectTasks.nextId)

  def outputArrived(state: AppState, id: Long, chunk: String): ReducerResult =
    current(state, id).fold(ReducerResult.noEffects(state)) { task =>
      val updated = task.copy(output = ProjectTaskRunner.appendOutputTail(task.output, chunk))
      showTerminal(
        withTasks(state, state.runtime.projectTasks.copy(running = Some(updated))),
        ProjectTaskTerminal.running(task.command, updated.output)
      )
    }

  def finished(state: AppState, id: Long, outcome: Either[Throwable, ProjectTaskResult]): ReducerResult =
    current(state, id).fold(ReducerResult.noEffects(state)) { task =>
      val text = outcome.fold(ProjectTaskTerminal.failedToStart(task.command, _), ProjectTaskTerminal.completed)
      showTerminal(released(state), text)
    }

  private def current(state: AppState, id: Long): Option[RunningProjectTask] =
    state.runtime.projectTasks.running.filter(_.id == id)

  // A result can re-pin a terminal panel closed some other way than unpinning it, so it animates like any other pin.
  private def showTerminal(state: AppState, text: String): ReducerResult =
    val pinned = PinnedPanelContentReducer.pinOrUpdateTerminal(text, TerminalPosition, TerminalSize, state)
    val animated =
      if AnimationChoreography.shouldApplySurfaceAnimationHooks(state) then
        AnimationChoreography.animateSurfaceTransitions(state, pinned.state).getOrElse(pinned.state)
      else pinned.state
    pinned.copy(state = animated)

  private def withTasks(state: AppState, tasks: ProjectTasks): AppState =
    state.copy(runtime = state.runtime.copy(projectTasks = tasks))
