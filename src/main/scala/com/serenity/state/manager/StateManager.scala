package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.annotation.unused

import cats.effect.*
import com.serenity.command.{Command, CommandRunner, CommandSurfaceItem}
import com.serenity.config.{AppConfig, PreferredWindowSize}
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.Event
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.renderer.RendererFrameState
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

trait EventApplier:
  def applyEvent(event: Event): IO[Unit]

trait StateReader:
  def getCurrentState: IO[AppState]

  /** One consistent snapshot of everything the dispatcher owns: use it wherever app state and buffer animations are
    * read together, since two separate reads can straddle a write.
    */
  def getModel: IO[Model]

trait StateUpdater:

  /** The result is checked by `AppStateValidation` before it commits -- an update that would leave the state invalid is
    * rejected and the state before the call is kept instead (#1183).
    */
  def updateStateValidated(update: AppState => AppState): IO[Unit]

/** The hot-path state engine: reading, mutating, and applying events to `AppState`.
  *
  * Deliberately a cohesive trait, NOT a capability record (#1017). `getCurrentState`/`getModel` are read on the
  * per-frame render path (`AppRuntime.fastRenderPhase`), which #1017's acceptance criteria carve out from record
  * conversion exactly as they do `RenderSurface` -- a trait method at a monomorphic call site stays JIT-inlinable where
  * a record's function field would not. The three capabilities remain as sub-traits so narrow consumers can still
  * depend on exactly what they use (`RenderController` on `EventApplier`, `ClipboardEventSync` on `StateReader`/
  * `StateUpdater`); `StateEngine` names their union so the hot core is a single first-class type rather than an
  * anonymous intersection. This is the record-of-records epic's one deliberate hot-core exception.
  */
trait StateEngine extends StateReader, StateUpdater, EventApplier

/** Advances renderer-visible animation state.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly.
  */
final case class AnimationTicker(advanceAnimationsOnTick: IO[Boolean])

/** Owns application shutdown and periodic session persistence.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly. `awaitQuit` and `intervalSaveStream` hold their
  * descriptions directly (same as `LspEffectSource.lspEffectStream`); `forceQuit` does too, since every call site
  * invokes it immediately rather than passing the unapplied function around, so a plain `IO[Unit]` field (not
  * `() => IO[Unit]`) matches how it's actually used despite the trait's empty-parens method having declared it as a
  * `def`.
  */
final case class RuntimeLifecycle(
    awaitQuit: IO[Unit],
    forceQuit: IO[Unit],
    intervalSaveStream: Stream[IO, Unit],
    /** Completes once lane work accepted so far (config/preset writes, searches) and the results it hands back have
      * settled. Event dispatch returns without waiting for that work, so this is the point to observe it.
      */
    awaitEffects: IO[Unit]
)

/** Supplies effects for the language-server interpreter.
  *
  * A `StateManager` capability-record slice (see #1017): a case class holding the stream description directly instead
  * of a trait mixed into `StateManager`.
  */
final case class LspEffectSource(lspEffectStream: Stream[IO, LspEffect])

/** Reads persisted session metadata needed before startup restoration.
  *
  * A cold capability expressed as a record of functions rather than a trait -- see #1017 and `BufferManager` above for
  * the shape rationale. `StateManager` holds one of these as a field (a "record of records") instead of mixing this
  * trait in directly.
  */
final case class SessionStartupInfo(
    currentSessionThemeName: IO[Option[String]],
    sessionExists: IO[Boolean]
)

/** Opens a file into editor state.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly.
  */
final case class FileOpener(openFile: Path => IO[Unit])

/** Manages editor buffers.
  *
  * A capability record per #1017: a case class of functions rather than a trait, one of `StateManager`'s "record of
  * records" slices from its original 18-trait facade -- `StateManager` holds one of these as a field instead of mixing
  * this trait in directly. The former default on `createBuffer`'s `filePath` parameter can't survive as a case class
  * field, so callers now pass `None` explicitly.
  */
final case class BufferManager(
    createBuffer: (String, Option[Path]) => IO[BufferId],
    createNewEmptyBuffer: IO[BufferId],
    updateBuffer: (BufferId, String) => IO[Unit]
)

/** Manages editor panes, tabs, and splits.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly. Its methods are split across the viewport and editor
  * capability classes, so the record is assembled in `StateManagerComposition` rather than in a single class. Case
  * class fields can't carry default parameter values, so `createPane` takes `Option[BufferId]` rather than defaulting
  * it to `None`.
  */
final case class PaneManager(
    handleViewportResize: ViewportSize => IO[Unit],
    createPane: Option[BufferId] => IO[PaneId],
    switchToPane: PaneId => IO[Unit],
    getTabOrder: () => IO[List[PaneId]]
)

/** Reads the persisted editor session.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly.
  */
final case class SessionService(loadSession: IO[Option[AppState]])

/** Manages pinned panels and the file explorer.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly.
  */
final case class PanelManager(
    pinPanel: (PanelContent, PanelPosition, Int) => IO[Unit],
    pinOrUpdateTerminalPanel: (String, PanelPosition, Int) => IO[Unit],
    unpinPanel: PanelTarget => IO[Unit],
    movePinnedPanel: (SurfaceId, PanelPosition) => IO[Unit],
    expandPinnedPanel: PanelTarget => IO[Unit],
    collapseExpandedPanel: () => IO[Unit],
    switchToPinnedPanel: PanelTarget => IO[Unit],
    loadDirectoryTree: (Path, List[String]) => IO[Unit],
    selectFileInExplorer: Path => IO[Unit],
    resizePinnedPanel: (PanelTarget, Int) => IO[Unit],
    dragFileToDirectory: (Path, Path) => IO[Unit]
)

/** Saves buffers and watches their files for changes made outside the editor.
  *
  * A capability record per #1017 -- see `BufferManager` above for the shape rationale. `StateManager` holds one of
  * these as a field instead of mixing this trait in directly.
  */
final case class FileService(
    saveBuffer: BufferId => IO[Unit],
    saveBufferAs: (BufferId, Path) => IO[Unit],
    // #1623: re-checks the focused buffer's on-disk revision on window focus-gain, called from AppRuntime's focus
    // callback -- see StateManagerEffectHandlers.resolveExternalRevisionEffect for the reload-or-prompt logic.
    checkExternalChangesOnFocus: IO[Unit],
    // #1623: the background counterpart -- AppRuntime's FileChangeWatcher poll loop calls openBufferPaths each cycle
    // to keep its watched directory set current, then checkBufferForExternalChanges for whichever buffers' files a
    // poll window actually saw change.
    openBufferPaths: IO[Map[Path, BufferId]],
    checkBufferForExternalChanges: BufferId => IO[Unit]
)

trait StateManager extends StateEngine:
  def sessionStartupInfo: SessionStartupInfo
  def lspEffectSource: LspEffectSource
  def runtimeLifecycle: RuntimeLifecycle
  def sessionService: SessionService
  def animationTicker: AnimationTicker
  def bufferManager: BufferManager
  def panelManager: PanelManager
  def fileOpener: FileOpener
  def fileService: FileService
  def paneManager: PaneManager

  /** Runs a `Command` directly, bypassing the palette's UI wiring -- a test-harness concern (#1724), not a production
    * capability: nothing outside `state.manager` calls this, so it stays package-private rather than a record field
    * (`CommandExecutor`, deleted in #1724) on the public façade. `StateManagerTestFacade.executeCommand` reaches it for
    * specs.
    */
  private[manager] def executeCommand(command: Command): IO[Unit]

object StateManager:

  private val EphemeralSessionProperty = "serenity.test.ephemeralSessions"

  private def resolveSessionRootOverride(sessionRootOverride: Option[Path]): IO[Option[Path]] =
    sessionRootOverride match
      case some @ Some(_) =>
        IO.pure(some)
      case None if java.lang.Boolean.getBoolean(EphemeralSessionProperty) =>
        IO.blocking(Some(Files.createTempDirectory("serenity-state-manager-test")))
      case None =>
        IO.pure(None)

  def apply(
    @unused parentLogger: Logger[IO],
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy(),
    onFontConfigChanged: FontConfig => IO[Unit] = _ => IO.unit,
    deviceTextScaleProvider: IO[Double] = IO.pure(1.0),
    sessionRootOverride: Option[Path] = None,
    initialConfig: AppConfig = AppConfig.default,
    configPersistencePath: Option[Path] = None,
    uiPresetStore: UiPresetStore = UiPresetStore.default,
    windowSizeProvider: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    fileDialog: Option[FileDialog] = None,
    markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability =
      com.serenity.ui.tui.MarkdownPreviewWindowAvailability.Unavailable
  )(using Balance, LoggerFactory[IO]): IO[StateManager] =
    val themeManager = AppThemeManager.create
    RendererFrameState.configureCacheCapacity(initialConfig.surfaceConfig.rendererFrameStateCacheCapacity)
    for
      resolvedSessionRootOverride <- resolveSessionRootOverride(sessionRootOverride)
      themeNames                  <- themeManager.listAvailableThemes.handleErrorWith(_ => IO.pure(Nil))
      initialState = AppState.initial(initialConfig)
      modelRef <- Ref.of[IO, Model](
        Model(
          app = initialState.copy(runtime = initialState.runtime.copy(availableThemeNames = themeNames)),
          undo = UndoState(maxUndoDepth = policy.maxUndoDepth),
          bufferAnimations = Map.empty
        )
      )
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      themeNamesRef       <- Ref.of[IO, List[String]](themeNames)
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      runtime = StateManagerRuntime.create(
        modelRef = modelRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = LoggerFactory[IO].getLogger(using LoggerName("com.serenity.state.manager.StateManager")),
        policy = policy,
        sessionRootOverride = resolvedSessionRootOverride,
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
        markdownPreviewWindow = markdownPreviewWindow
      )
      stateManager <- fromRuntime(runtime)
    yield stateManager

  /** Assembles a state manager over an already-built runtime -- the seam specs use to substitute infrastructure such as
    * a gated `FileManager`.
    */
  private[manager] def fromRuntime(runtime: StateManagerRuntime)(using Balance): IO[StateManager] =
    StateManagerOperationBoundary
      .create(runtime.modelRef, runtime.logger)
      .map(operations => new StateManagerImpl(runtime, operations))

  def describeCommandRunnerEvent(event: Event, runner: CommandRunner): String =
    // issue #931: category tabs (and the `activeCategory` field they drove) are retired, so this no longer names a
    // category -- just whether there's a live search.
    val modePart =
      if runner.searchTerm.isEmpty then "mode=browse" else "mode=search"
    val selectedPart =
      runner.selectedItem match
        case Some(CommandSurfaceItem.CommandItem(command))    => s"selected=command:${command.name}"
        case Some(option: CommandSurfaceItem.OptionItem)      => s"selected=option:${option.id}"
        case Some(toggle: CommandSurfaceItem.ToggleItem)      => s"selected=toggle:${toggle.id}"
        case Some(item: CommandSurfaceItem.InputItem)         => s"selected=input:${item.id}"
        case Some(item: CommandSurfaceItem.SettingSearchItem) => s"selected=setting:${item.targetItemId}"
        case Some(group: CommandSurfaceItem.GroupItem)        => s"selected=group:${group.id}"
        case None                                             => "selected=none"

    s"event=${structuralName(event)} $modePart $selectedPart"

  def describeCommandExecution(command: Command): String =
    s"command=${command.name} category=${command.category} intent=${structuralName(command.intent)}"

  /** Structural, non-sensitive name for a domain event or command intent: the chain of case names down to (but never
    * including) any leaf payload. Recursion is restricted to our own sealed hierarchies (`com.serenity` types) so it
    * can never descend into a `String`, `Path`, `List`, or other free-form user-supplied value - those always stop the
    * walk at their enclosing case name. This keeps rolling logs identifying *what* happened without ever capturing
    * typed characters, search text, comments, dictionary words/paths, preset names, or key bindings.
    */
  private def structuralName(value: Any): String =
    value match
      case p: Product if p.productArity == 1 =>
        p.productElement(0) match
          case nested: Product if nested.getClass.getName.startsWith("com.serenity") =>
            s"${p.productPrefix}(${structuralName(nested)})"
          case _ => p.productPrefix
      case p: Product => p.productPrefix
      case other      => other.getClass.getSimpleName

  private class StateManagerImpl(runtime: StateManagerRuntime, operations: StateManagerOperationBoundary)(using Balance)
      extends StateManager:

    private val composition = new StateManagerComposition(
      runtime.themeNamesRef,
      runtime.quitSignal,
      runtime.logger,
      runtime.policy,
      runtime.themeManager,
      runtime.lspQueue,
      runtime.mouseTargetCacheRef,
      runtime.onFontConfigChanged,
      runtime.deviceTextScaleProvider,
      runtime.configPersistencePath,
      runtime.uiPresetStore,
      runtime.windowSizeProvider,
      runtime.fileDialog,
      runtime.markdownPreviewWindow,
      runtime.runProjectTask,
      runtime.fileManager,
      runtime.sessionManager,
      runtime.sessionPersistence,
      operations
    )

    export composition.*
