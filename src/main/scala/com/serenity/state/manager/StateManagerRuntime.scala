package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.*
import cats.effect.std.{Queue, Semaphore}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{FileDialog, FileManager}
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
import com.serenity.state.models.AppState
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import org.typelevel.log4cats.Logger

/** Non-blocking, coalescing hand-off from editor state changes to the LSP runtime. */
final private[manager] class LspEffectQueue private (
    queue: Queue[IO, LspEffectQueue.Entry],
    pendingChanges: Ref[IO, Map[String, LspEffectQueue.PendingChange]],
    documentVersions: Ref[IO, Map[String, Int]]
):

  import LspEffectQueue.*

  def enqueue(effect: LspEffect): IO[Unit] =
    effect match
      case LspEffect.FileChanged(uri, languageId, text, _) => enqueueDocumentChange(uri, languageId, text)
      case other                                           => queue.offer(Entry.Immediate(other))

  def enqueueDocumentChange(uri: String, languageId: com.serenity.lsp.config.LanguageId, text: String): IO[Unit] =
    pendingChanges.modify { changes =>
      if changes.contains(uri) then (changes.updated(uri, PendingChange(languageId, text)), IO.unit)
      else
        (
          changes.updated(uri, PendingChange(languageId, text)),
          queue.offer(Entry.Change(uri))
        )
    }.flatten

  def stream: Stream[IO, LspEffect] =
    Stream.repeatEval(take)

  private def take: IO[LspEffect] =
    queue.take.flatMap {
      case Entry.Immediate(opened @ LspEffect.FileOpened(uri, _, _)) =>
        documentVersions.update(_ + (uri -> 1)).as(opened)
      case Entry.Immediate(closed @ LspEffect.FileClosed(uri, _)) =>
        documentVersions.update(_ - uri).as(closed)
      case Entry.Immediate(effect) =>
        IO.pure(effect)
      case Entry.Change(uri) =>
        pendingChanges
          .modify(changes => (changes - uri, changes.get(uri)))
          .flatMap {
            case Some(PendingChange(languageId, text)) =>
              documentVersions.modify { versions =>
                val version = versions.getOrElse(uri, 1) + 1
                (versions.updated(uri, version), LspEffect.FileChanged(uri, languageId, text, version))
              }
            case None =>
              take
          }
    }

private[manager] object LspEffectQueue:

  private enum Entry:
    case Immediate(effect: LspEffect)
    case Change(uri: String)

  private case class PendingChange(languageId: com.serenity.lsp.config.LanguageId, text: String)

  def create: IO[LspEffectQueue] =
    for
      queue            <- Queue.unbounded[IO, Entry]
      pendingChanges   <- Ref.of[IO, Map[String, PendingChange]](Map.empty)
      documentVersions <- Ref.of[IO, Map[String, Int]](Map.empty)
    yield new LspEffectQueue(queue, pendingChanges, documentVersions)

private[manager] case class ManagedProjectTask(
    finished: Deferred[IO, Unit],
    fiber: Fiber[IO, Throwable, Unit]
)

private[manager] object ProjectTaskOwnership:

  def clear(
    projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
    finished: Deferred[IO, Unit]
  ): IO[Unit] =
    projectTaskFiberRef.update(_.filterNot(_.finished eq finished))

  def cancel(
    projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
    projectTaskSemaphore: Semaphore[IO]
  ): IO[Boolean] =
    projectTaskSemaphore.permit.use(
      projectTaskFiberRef.getAndSet(None).flatMap {
        case Some(task) => task.fiber.cancel.as(true)
        case None       => IO.pure(false)
      }
    )

private[manager] case class StateManagerRuntime(
    stateRef: Ref[IO, AppState],
    undoRef: Ref[IO, UndoState],
    themeNamesRef: Ref[IO, List[String]],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy,
    themeManager: AppThemeManager,
    lspQueue: LspEffectQueue,
    projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
    projectTaskSemaphore: Semaphore[IO],
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    onFontConfigChanged: FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
    fileDialog: FileDialog,
    fileManager: FileManager,
    sessionManager: SessionManager,
    sessionPersistence: SessionPersistence
)

private[manager] object StateManagerRuntime:

  def create(
    stateRef: Ref[IO, AppState],
    undoRef: Ref[IO, UndoState],
    themeNamesRef: Ref[IO, List[String]],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy,
    sessionRootOverride: Option[Path],
    themeManager: AppThemeManager,
    lspQueue: LspEffectQueue,
    projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]],
    projectTaskSemaphore: Semaphore[IO],
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    onFontConfigChanged: FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
    fileDialog: FileDialog
  )(using Balance): StateManagerRuntime =
    val sessionManager = sessionRootOverride
      .map(root => SessionManager.create(root, themeManager, logger, policy))
      .getOrElse(SessionManager.create(themeManager, logger, policy))
    StateManagerRuntime(
      stateRef = stateRef,
      undoRef = undoRef,
      themeNamesRef = themeNamesRef,
      quitSignal = quitSignal,
      logger = logger,
      policy = policy,
      themeManager = themeManager,
      lspQueue = lspQueue,
      projectTaskFiberRef = projectTaskFiberRef,
      projectTaskSemaphore = projectTaskSemaphore,
      mouseTargetCacheRef = mouseTargetCacheRef,
      documentAnalysisFiberRef = documentAnalysisFiberRef,
      onFontConfigChanged = onFontConfigChanged,
      deviceTextScaleProvider = deviceTextScaleProvider,
      configPersistencePath = configPersistencePath,
      uiPresetStore = uiPresetStore,
      windowSizeProvider = windowSizeProvider,
      onPreferredWindowSizeChanged = onPreferredWindowSizeChanged,
      fileDialog = fileDialog,
      fileManager = new FileManager(),
      sessionManager = sessionManager,
      sessionPersistence = new SessionPersistence(sessionManager, policy, logger)
    )
