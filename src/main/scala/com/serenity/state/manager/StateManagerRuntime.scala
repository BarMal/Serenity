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

/** Non-blocking, coalescing hand-off from editor state changes to the LSP runtime -- the LSP lane of #1697. It is
  * already the lane's shape, so it is not routed through `EffectLanes`: one FIFO drained by `LspManager`'s single
  * consumer keeps every server's notifications and requests in the order they were enqueued, and results come back
  * through `applyEvent` on the dispatcher. An edit coalesces into a change still queued for its document only when
  * nothing else for that document was queued after it, so coalescing never moves an edit ahead of a request, a close or
  * a reopen.
  */
final private[manager] class LspEffectQueue private (
    queue: Queue[IO, LspEffectQueue.Entry],
    pendingChanges: Ref[IO, LspEffectQueue.PendingChanges],
    documentVersions: Ref[IO, Map[String, Int]]
):

  import LspEffectQueue.*

  def enqueue(effect: LspEffect): IO[Unit] =
    effect match
      case LspEffect.FileChanged(uri, languageId, text, _) => enqueueDocumentChange(uri, languageId, text)
      case other => pendingChanges.update(_.closedFor(other.uri)) >> queue.offer(Entry.Immediate(other))

  def enqueueDocumentChange(uri: String, languageId: com.serenity.lsp.config.LanguageId, text: String): IO[Unit] =
    pendingChanges.modify { pending =>
      val change = PendingChange(languageId, text)
      pending.open.get(uri) match
        case Some(token) => (pending.copy(texts = pending.texts.updated(token, change)), IO.unit)
        case None =>
          val token = pending.nextToken
          (
            PendingChanges(token + 1, pending.open.updated(uri, token), pending.texts.updated(token, change)),
            queue.offer(Entry.Change(uri, token))
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
      case Entry.Change(uri, token) =>
        pendingChanges
          .modify(pending => (pending.taken(uri, token), pending.texts.get(token)))
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
    case Change(uri: String, token: Long)

  final private case class PendingChange(languageId: com.serenity.lsp.config.LanguageId, text: String)

  /** Queued changes' latest text by token; `open` names, per document, the queued change a new edit may still join. */
  final private case class PendingChanges(nextToken: Long, open: Map[String, Long], texts: Map[Long, PendingChange]):
    def closedFor(uri: String): PendingChanges = copy(open = open - uri)

    def taken(uri: String, token: Long): PendingChanges =
      copy(open = if open.get(uri).contains(token) then open - uri else open, texts = texts - token)

  def create: IO[LspEffectQueue] =
    for
      queue            <- Queue.unbounded[IO, Entry]
      pendingChanges   <- Ref.of[IO, PendingChanges](PendingChanges(0L, Map.empty, Map.empty))
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
