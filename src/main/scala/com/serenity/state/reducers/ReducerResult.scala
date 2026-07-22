package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.command.Command
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.{AppState, BufferId, SurfaceId}
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.theme.config.ThemeConfig

enum LifecycleEffect:
  case CompleteQuit

enum CommandEffect:
  case Execute(command: Command)

enum ThemeEffect:
  case SwitchTheme(themeName: String)
  case ReloadTheme(themeName: String)
  case SaveThemeConfig(config: ThemeConfig)

enum SurfaceEffect:
  case OpenThemePicker
  case OpenThemeCreator
  case OpenFileSearch

enum FileEffect:
  case SaveBuffer(bufferId: BufferId)
  case SaveBufferAs(bufferId: BufferId, path: Path)
  case DirectLoadFile(path: Path)

enum ExplorerEffect:
  case OpenRoot(position: PanelPosition, path: Path, size: Int)
  case LoadDirectory(position: PanelPosition, path: Path)

enum WorkflowEffect:
  case RequestOpenFile
  case RequestSaveAs
  case RefreshFileWorkflow(surfaceId: SurfaceId)
  case SubmitFileWorkflow(surfaceId: SurfaceId)
  case SubmitReplaceWorkflow(surfaceId: SurfaceId)
  case SubmitCloseWorkflow(surfaceId: SurfaceId)

enum LspQueueEffect:
  case Enqueue(effect: LspEffect)
  case DocumentChanged(uri: String, languageId: LanguageId, text: String)

enum AppEffect:
  case Lifecycle(effect: LifecycleEffect)
  case CommandRequest(effect: CommandEffect)
  case Theme(effect: ThemeEffect)
  case Surface(effect: SurfaceEffect)
  case File(effect: FileEffect)
  case Explorer(effect: ExplorerEffect)
  case Workflow(effect: WorkflowEffect)
  case LspQueue(effect: LspQueueEffect)

object AppEffect:

  object CompleteQuit:
    def apply(): AppEffect = AppEffect.Lifecycle(LifecycleEffect.CompleteQuit)

    def unapply(effect: AppEffect): Boolean =
      effect match
        case AppEffect.Lifecycle(LifecycleEffect.CompleteQuit) => true
        case _                                                 => false

  object ExecuteCommand:
    def apply(command: Command): AppEffect =
      AppEffect.CommandRequest(CommandEffect.Execute(command))

    def unapply(effect: AppEffect): Option[Command] =
      effect match
        case AppEffect.CommandRequest(CommandEffect.Execute(command)) => Some(command)
        case _                                                        => None

  object SwitchTheme:
    def apply(themeName: String): AppEffect =
      AppEffect.Theme(ThemeEffect.SwitchTheme(themeName))

    def unapply(effect: AppEffect): Option[String] =
      effect match
        case AppEffect.Theme(ThemeEffect.SwitchTheme(themeName)) => Some(themeName)
        case _                                                   => None

  object ReloadTheme:
    def apply(themeName: String): AppEffect =
      AppEffect.Theme(ThemeEffect.ReloadTheme(themeName))

    def unapply(effect: AppEffect): Option[String] =
      effect match
        case AppEffect.Theme(ThemeEffect.ReloadTheme(themeName)) => Some(themeName)
        case _                                                   => None

  object SaveThemeConfig:
    def apply(config: ThemeConfig): AppEffect =
      AppEffect.Theme(ThemeEffect.SaveThemeConfig(config))

    def unapply(effect: AppEffect): Option[ThemeConfig] =
      effect match
        case AppEffect.Theme(ThemeEffect.SaveThemeConfig(config)) => Some(config)
        case _                                                    => None

  object SaveBuffer:
    def apply(bufferId: BufferId): AppEffect =
      AppEffect.File(FileEffect.SaveBuffer(bufferId))

    def unapply(effect: AppEffect): Option[BufferId] =
      effect match
        case AppEffect.File(FileEffect.SaveBuffer(bufferId)) => Some(bufferId)
        case _                                               => None

  object SaveBufferAs:
    def apply(bufferId: BufferId, path: Path): AppEffect =
      AppEffect.File(FileEffect.SaveBufferAs(bufferId, path))

    def unapply(effect: AppEffect): Option[(BufferId, Path)] =
      effect match
        case AppEffect.File(FileEffect.SaveBufferAs(bufferId, path)) => Some((bufferId, path))
        case _                                                       => None

  object RequestOpenFile:
    def apply(): AppEffect =
      AppEffect.Workflow(WorkflowEffect.RequestOpenFile)

    def unapply(effect: AppEffect): Boolean =
      effect match
        case AppEffect.Workflow(WorkflowEffect.RequestOpenFile) => true
        case _                                                  => false

  object RequestSaveAs:
    def apply(): AppEffect =
      AppEffect.Workflow(WorkflowEffect.RequestSaveAs)

    def unapply(effect: AppEffect): Boolean =
      effect match
        case AppEffect.Workflow(WorkflowEffect.RequestSaveAs) => true
        case _                                                => false

  object DirectLoadFile:
    def apply(path: Path): AppEffect =
      AppEffect.File(FileEffect.DirectLoadFile(path))

    def unapply(effect: AppEffect): Option[Path] =
      effect match
        case AppEffect.File(FileEffect.DirectLoadFile(path)) => Some(path)
        case _                                               => None

  object LoadPinnedDirectory:
    def apply(position: PanelPosition, path: Path): AppEffect =
      AppEffect.Explorer(ExplorerEffect.LoadDirectory(position, path))

    def unapply(effect: AppEffect): Option[(PanelPosition, Path)] =
      effect match
        case AppEffect.Explorer(ExplorerEffect.LoadDirectory(position, path)) => Some((position, path))
        case _                                                                => None

  object OpenThemePicker:
    def apply(): AppEffect =
      AppEffect.Surface(SurfaceEffect.OpenThemePicker)

    def unapply(effect: AppEffect): Boolean =
      effect match
        case AppEffect.Surface(SurfaceEffect.OpenThemePicker) => true
        case _                                                => false

  object OpenThemeCreator:
    def apply(): AppEffect =
      AppEffect.Surface(SurfaceEffect.OpenThemeCreator)

    def unapply(effect: AppEffect): Boolean =
      effect match
        case AppEffect.Surface(SurfaceEffect.OpenThemeCreator) => true
        case _                                                 => false

  object OpenFileSearch:
    def apply(): AppEffect =
      AppEffect.Surface(SurfaceEffect.OpenFileSearch)

    def unapply(effect: AppEffect): Boolean =
      effect match
        case AppEffect.Surface(SurfaceEffect.OpenFileSearch) => true
        case _                                               => false

  object RefreshFileWorkflow:
    def apply(surfaceId: SurfaceId): AppEffect =
      AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(surfaceId))

    def unapply(effect: AppEffect): Option[SurfaceId] =
      effect match
        case AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(surfaceId)) => Some(surfaceId)
        case _                                                                 => None

  object SubmitFileWorkflow:
    def apply(surfaceId: SurfaceId): AppEffect =
      AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(surfaceId))

    def unapply(effect: AppEffect): Option[SurfaceId] =
      effect match
        case AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(surfaceId)) => Some(surfaceId)
        case _                                                                => None

  object SubmitReplaceWorkflow:
    def apply(surfaceId: SurfaceId): AppEffect =
      AppEffect.Workflow(WorkflowEffect.SubmitReplaceWorkflow(surfaceId))

    def unapply(effect: AppEffect): Option[SurfaceId] =
      effect match
        case AppEffect.Workflow(WorkflowEffect.SubmitReplaceWorkflow(surfaceId)) => Some(surfaceId)
        case _                                                                   => None

  object SubmitCloseWorkflow:
    def apply(surfaceId: SurfaceId): AppEffect =
      AppEffect.Workflow(WorkflowEffect.SubmitCloseWorkflow(surfaceId))

    def unapply(effect: AppEffect): Option[SurfaceId] =
      effect match
        case AppEffect.Workflow(WorkflowEffect.SubmitCloseWorkflow(surfaceId)) => Some(surfaceId)
        case _                                                                 => None

  object EnqueueLspEffect:
    def apply(effect: LspEffect): AppEffect =
      AppEffect.LspQueue(LspQueueEffect.Enqueue(effect))

    def unapply(appEffect: AppEffect): Option[LspEffect] =
      appEffect match
        case AppEffect.LspQueue(LspQueueEffect.Enqueue(effect)) => Some(effect)
        case _                                                  => None

case class ReducerResult(
    state: AppState,
    effects: List[AppEffect] = Nil
)

object ReducerResult:
  def noEffects(state: AppState): ReducerResult =
    ReducerResult(state, Nil)

  def withEffect(state: AppState, effect: AppEffect): ReducerResult =
    ReducerResult(state, List(effect))
