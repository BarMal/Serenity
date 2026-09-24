package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.{LspIntent, ProjectIntent}
import com.serenity.io.FileUtils
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.*
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*

/** Project-task execution (run/cancel a detected build/test command, piping its output into a pinned terminal panel)
  * and LSP request dispatch (hover/completion/definition) for the focused buffer.
  *
  * A task runs on the switch-latest [[StateManagerProjectLspEffects.TaskLane]] (#1697): the state decides that only one
  * runs at a time, so the lane never has to drop a task the state accepted; cancelling is superseding it with an empty
  * job; and quitting cancels it -- destroying its process -- rather than waiting for a build to finish. Its output
  * comes back as versioned results, applied only while it is still the task the terminal panel shows.
  */
final private[manager] class StateManagerProjectLspEffects(
    lspQueue: LspEffectQueue,
    currentState: IO[AppState],
    commitApp: (AppState => AppState) => IO[Unit],
    lanes: EffectLanePort,
    launchTask: ProjectTaskLauncher,
    pinOrUpdateTerminalPanel: (String, com.serenity.ui.layout.PanelPosition, Int) => IO[Unit],
    showPeek: (com.serenity.ui.layout.PeekContent, CursorPosition) => IO[Unit],
    showModal: Modal => IO[Unit]
):
  import StateManagerProjectLspEffects.*

  private[manager] def interpretProject(intent: ProjectIntent, state: AppState): IO[Unit] =
    intent match
      case ProjectIntent.RunProjectTask(kind) =>
        runProjectTask(state, kind)
      case ProjectIntent.CancelProjectTask =>
        cancelProjectTask

  private[manager] def interpretLsp(intent: LspIntent, state: AppState): IO[Unit] =
    intent match
      case LspIntent.RequestLspHover =>
        requestLspHover(state)
      case LspIntent.RequestLspCompletion =>
        requestLspCompletion(state)
      case LspIntent.RequestLspDefinition =>
        requestLspDefinition(state)
      case LspIntent.RequestLspReferences =>
        requestLspReferences(state)
      case LspIntent.OpenRenameSymbolPrompt =>
        openRenameSymbolPrompt(state)

  private def runProjectTask(state: AppState, kind: ProjectTaskKind): IO[Unit] =
    if !state.editingContext.hasCodeTooling then pinProjectTerminal(ProjectTaskTerminal.notAvailableInProseMode(kind))
    else
      projectTaskStartPath(state).flatMap { start =>
        ProjectTaskDetector.detect(start, kind) match
          case None =>
            pinProjectTerminal(ProjectTaskTerminal.noTask(kind, start))
          case Some(command) =>
            startProjectTask(command)
      }

  private def startProjectTask(command: ProjectTaskCommand): IO[Unit] =
    currentState.map(_.runtime.projectTasks).flatMap { tasks =>
      val id = tasks.nextId
      if tasks.running.isDefined then pinProjectTerminal(AlreadyRunning)
      else
        commitApp(ProjectTaskTransitions.claimed(_, id, command)) >>
          currentState.map(_.runtime.projectTasks.running.exists(_.id == id)).flatMap { claimed =>
            if claimed then
              pinProjectTerminal(ProjectTaskTerminal.started(command)) >>
                lanes.submitEffect(TaskLane, taskJob(id, command))
            else pinProjectTerminal(AlreadyRunning)
          }
    }

  /** Reads the process output into a local buffer and hands it to the dispatcher at most once per
    * [[OutputPublishInterval]], however fast the process writes: each hand-off waits for the dispatcher, so a busy
    * dispatcher only makes the next batch bigger. The buffer keeps the same bounded tail the panel does.
    */
  private def taskJob(id: Long, command: ProjectTaskCommand): IO[Unit] =
    IO.ref("").flatMap { unpublished =>
      val publishOutput =
        unpublished
          .getAndSet("")
          .flatMap(chunk =>
            lanes.dispatchEffectResult(EffectResult.ProjectTaskOutput(id, chunk), _ => IO.unit).whenA(chunk.nonEmpty)
          )
      (IO.sleep(OutputPublishInterval) >> publishOutput).foreverM.background
        .surround(
          launchTask(command, chunk => unpublished.update(ProjectTaskRunner.appendOutputTail(_, chunk))).attempt
        )
        .flatMap(outcome => lanes.dispatchEffectResult(EffectResult.ProjectTaskFinished(id, outcome), _ => IO.unit))
    }

  private def projectTaskStartPath(state: AppState): IO[Path] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .flatMap(_.document.filePath)
      .fold(FileUtils.getCurrentDirectory)(path => IO.pure(path))

  private def pinProjectTerminal(text: String): IO[Unit] =
    pinOrUpdateTerminalPanel(text, ProjectTaskTransitions.TerminalPosition, ProjectTaskTransitions.TerminalSize)

  private def cancelProjectTask: IO[Unit] =
    currentState.map(_.runtime.projectTasks.running.isDefined).flatMap { wasRunning =>
      cancelProjectTaskSilently >>
        pinProjectTerminal(if wasRunning then "Project task cancelled." else "No project task is running.")
    }

  /** Same cancellation as `cancelProjectTask`, without the confirmation pin -- for closing the output panel itself
    * (issue #1294), where re-pinning a "cancelled" message would immediately undo the close. Supersedes the lane even
    * when the state records no task, so a process whose record was lost (a session restore) still stops.
    */
  private[manager] def cancelProjectTaskSilently: IO[Unit] =
    commitApp(ProjectTaskTransitions.released) >> lanes.submitEffect(TaskLane, IO.unit)

  private def requestLspHover(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, _)) =>
        lspQueue.enqueue(LspEffect.HoverRequested(uri, languageId, cursor.line, cursor.column, cursor))
      case None =>
        showLspUnavailablePeek(state)

  private def requestLspCompletion(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, _)) =>
        lspQueue.enqueue(LspEffect.CompletionRequested(uri, languageId, cursor.line, cursor.column, cursor))
      case None =>
        showLspUnavailablePeek(state)

  private def requestLspDefinition(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, buffer)) =>
        lspQueue.enqueue(
          LspEffect.DefinitionRequested(
            uri,
            languageId,
            cursor.line,
            cursor.column,
            cursor,
            wordAtCursor(buffer, cursor)
          )
        )
      case None =>
        showLspUnavailablePeek(state)

  private def requestLspReferences(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, buffer)) =>
        lspQueue.enqueue(
          LspEffect.ReferencesRequested(
            uri,
            languageId,
            cursor.line,
            cursor.column,
            cursor,
            wordAtCursor(buffer, cursor)
          )
        )
      case None =>
        showLspUnavailablePeek(state)

  private def openRenameSymbolPrompt(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, buffer)) =>
        showModal(
          Modal.RenameSymbol(uri, languageId, cursor.line, cursor.column, cursor, wordAtCursor(buffer, cursor))
        )
      case None =>
        showLspUnavailablePeek(state)

  private def activeLspRequestTarget(state: AppState): Option[(String, LanguageId, CursorPosition, Buffer)] =
    for
      bufferId <- state.persisted.layout.activeEditorPaneId
        .flatMap(state.persisted.layout.editorPanes.get)
        .flatMap(_.bufferId)
      buffer     <- state.persisted.buffers.get(bufferId)
      path       <- buffer.document.filePath
      languageId <- buffer.document.language
      cursor     <- buffer.editing.cursorPositions.headOption
    yield (path.toUri.toString, languageId, cursor, buffer)

  private def showLspUnavailablePeek(state: AppState): IO[Unit] =
    showPeek(
      com.serenity.ui.layout.PeekContent.QuickInfo("LSP requests need a saved buffer with a language mode."),
      state.activeCursorPosition.getOrElse(CursorPosition(0, 0))
    )

  private def wordAtCursor(buffer: Buffer, cursor: CursorPosition): String =
    val line = buffer.document.content.getLine(cursor.line).getOrElse("")
    if line.isEmpty then ""
    else
      val clamped = cursor.column.max(0).min(line.length)
      val start =
        Iterator.iterate(clamped)(i => i - 1).dropWhile(i => i > 0 && isSymbolChar(line.charAt(i - 1))).next()
      val end =
        Iterator.iterate(clamped)(i => i + 1).dropWhile(i => i < line.length && isSymbolChar(line.charAt(i))).next()
      line.substring(start, end)

  private def isSymbolChar(char: Char): Boolean =
    char.isLetterOrDigit || char == '_'

private[manager] object StateManagerProjectLspEffects:

  val TaskLane: Lane.Keyed = Lane.Keyed(LaneKey.Project, LanePolicy.SwitchLatest)

  // The refresh cadence the terminal panel has always had.
  val OutputPublishInterval: FiniteDuration = 100.millis

  private val AlreadyRunning = "A project task is already running. Use Cancel Project Task before starting another."
