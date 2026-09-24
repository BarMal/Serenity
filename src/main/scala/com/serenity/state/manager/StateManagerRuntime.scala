package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.*
import cats.effect.std.Queue
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{FileDialog, FileManager}
import com.serenity.lsp.LspEffect
import com.serenity.project.{ProjectTaskCommand, ProjectTaskResult, ProjectTaskRunner}
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence}
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

  final private case class PendingChange(languageId: com.serenity.lsp.config.LanguageId, text: String)

  def create: IO[LspEffectQueue] =
    for
      queue            <- Queue.unbounded[IO, Entry]
      pendingChanges   <- Ref.of[IO, Map[String, PendingChange]](Map.empty)
      documentVersions <- Ref.of[IO, Map[String, Int]](Map.empty)
    yield new LspEffectQueue(queue, pendingChanges, documentVersions)

/** Runs a project task, handing each piece of its output to the callback as it arrives. */
private[manager] type ProjectTaskLauncher = (ProjectTaskCommand, String => IO[Unit]) => IO[ProjectTaskResult]

final private[manager] case class StateManagerRuntime(
    modelRef: Ref[IO, Model],
    themeNamesRef: Ref[IO, List[String]],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy,
    themeManager: AppThemeManager,
    lspQueue: LspEffectQueue,
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    onFontConfigChanged: FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
    fileDialog: Option[FileDialog],
    markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability,
    runProjectTask: ProjectTaskLauncher,
    fileManager: FileManager,
    sessionManager: SessionManager,
    sessionPersistence: SessionPersistence
)

private[manager] object StateManagerRuntime:

  def create(
    modelRef: Ref[IO, Model],
    themeNamesRef: Ref[IO, List[String]],
    quitSignal: Deferred[IO, Unit],
    logger: Logger[IO],
    policy: SessionManager.SessionPolicy,
    sessionRootOverride: Option[Path],
    themeManager: AppThemeManager,
    lspQueue: LspEffectQueue,
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]],
    onFontConfigChanged: FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    configPersistencePath: Option[Path],
    uiPresetStore: UiPresetStore,
    windowSizeProvider: IO[Option[PreferredWindowSize]],
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit],
    fileDialog: Option[FileDialog],
    markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability =
      com.serenity.ui.tui.MarkdownPreviewWindowAvailability.Unavailable
  )(using Balance): StateManagerRuntime =
    val sessionManager = sessionRootOverride
      .map(root => SessionManager.create(root, themeManager, logger, policy))
      .getOrElse(SessionManager.create(themeManager, logger, policy))
    StateManagerRuntime(
      modelRef = modelRef,
      themeNamesRef = themeNamesRef,
      quitSignal = quitSignal,
      logger = logger,
      policy = policy,
      themeManager = themeManager,
      lspQueue = lspQueue,
      mouseTargetCacheRef = mouseTargetCacheRef,
      onFontConfigChanged = onFontConfigChanged,
      deviceTextScaleProvider = deviceTextScaleProvider,
      configPersistencePath = configPersistencePath,
      uiPresetStore = uiPresetStore,
      windowSizeProvider = windowSizeProvider,
      onPreferredWindowSizeChanged = onPreferredWindowSizeChanged,
      fileDialog = fileDialog,
      markdownPreviewWindow = markdownPreviewWindow,
      runProjectTask = (command, onOutput) => ProjectTaskRunner.runStreaming(command)(onOutput),
      fileManager = new FileManager(),
      sessionManager = sessionManager,
      sessionPersistence = new SessionPersistence(sessionManager, policy)
    )
