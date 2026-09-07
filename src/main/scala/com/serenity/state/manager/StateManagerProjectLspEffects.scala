package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.effect.std.Semaphore
import com.serenity.command.{LspIntent, ProjectIntent}
import com.serenity.io.FileUtils
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.*
import com.serenity.state.models.*
import fs2.Stream

/** Project-task execution (run/cancel a detected build/test command, piping its output into a pinned terminal
  * panel) and LSP request dispatch (hover/completion/definition) for the focused buffer.
  */
final private[manager] class StateManagerProjectLspEffects(
    stateRef: Ref[IO, AppState],
    lspQueue: LspEffectQueue,
    projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
    projectTaskSemaphore: Semaphore[IO],
    pinOrUpdateTerminalPanel: (String, com.serenity.ui.layout.PanelPosition, Int) => IO[Unit],
    showPeek: (com.serenity.ui.layout.PeekContent, CursorPosition) => IO[Unit]
):

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

  private def runProjectTask(state: AppState, kind: ProjectTaskKind): IO[Unit] =
    if state.persisted.config.appMode != com.serenity.config.AppMode.Code then
      pinProjectTerminal(ProjectTaskTerminal.notAvailableInProseMode(kind))
    else
      projectTaskStartPath(state).flatMap { start =>
        ProjectTaskDetector.detect(start, kind) match
          case None =>
            pinProjectTerminal(ProjectTaskTerminal.noTask(kind, start))
          case Some(command) =>
            startProjectTask(command)
      }

  private def startProjectTask(command: ProjectTaskCommand): IO[Unit] =
    projectTaskSemaphore.permit.use { _ =>
      projectTaskFiberRef.get.flatMap {
        case Some(_) =>
          pinProjectTerminal(
            "A project task is already running. Use Cancel Project Task before starting another."
          )
        case None =>
          for
            outputRef <- Ref.of[IO, String]("")
            finished  <- Deferred[IO, Unit]
            startTask <- Deferred[IO, Unit]
            renderer <- Stream
              .awakeEvery[IO](100.millis)
              .evalMap(_ =>
                outputRef.get.flatMap(output => pinProjectTerminal(ProjectTaskTerminal.running(command, output)))
              )
              .interruptWhen(Stream.eval(finished.get).as(true))
              .compile
              .drain
              .start
            task = startTask.get >> ProjectTaskRunner
              .runStreaming(command)(chunk =>
                outputRef.update(output => ProjectTaskRunner.appendOutputTail(output, chunk))
              )
              .attempt
              .flatMap {
                case Right(result) => pinProjectTerminal(ProjectTaskTerminal.completed(result))
                case Left(error)   => pinProjectTerminal(ProjectTaskTerminal.failedToStart(command, error))
              }
              .guarantee(
                finished.complete(()).attempt.void >> renderer.joinWithNever >> ProjectTaskOwnership
                  .clear(projectTaskFiberRef, finished)
              )
            fiber <- (pinProjectTerminal(ProjectTaskTerminal.started(command)) >> task).start
            _     <- projectTaskFiberRef.set(Some(ManagedProjectTask(finished, fiber)))
            _     <- startTask.complete(())
          yield ()
      }
    }

  private def projectTaskStartPath(state: AppState): IO[Path] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .flatMap(_.document.filePath)
      .fold(FileUtils.getCurrentDirectory)(path => IO.pure(path))

  private def pinProjectTerminal(text: String): IO[Unit] =
    pinOrUpdateTerminalPanel(text, com.serenity.ui.layout.PanelPosition.Bottom, 14)

  private def cancelProjectTask: IO[Unit] =
    ProjectTaskOwnership.cancel(projectTaskFiberRef, projectTaskSemaphore).flatMap {
      case true  => pinProjectTerminal("Project task cancelled.")
      case false => pinProjectTerminal("No project task is running.")
    }

  /** Same cancellation as `cancelProjectTask`, without the confirmation pin -- for closing the output panel itself
    * (issue #1294), where re-pinning a "cancelled" message would immediately undo the close.
    */
  private[manager] def cancelProjectTaskSilently: IO[Unit] =
    ProjectTaskOwnership.cancel(projectTaskFiberRef, projectTaskSemaphore).void

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

  private def activeLspRequestTarget(state: AppState): Option[(String, LanguageId, CursorPosition, Buffer)] =
    for
      bufferId <- state.persisted.layout.activeEditorPaneId
        .flatMap(state.persisted.layout.editorPanes.get)
        .flatMap(_.bufferId)
      buffer     <- state.persisted.buffers.get(bufferId)
      path       <- buffer.document.filePath
      languageId <- buffer.document.language
      cursor     <- buffer.editing.cursors.headOption
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
