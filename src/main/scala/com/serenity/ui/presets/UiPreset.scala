package com.serenity.ui.presets

import java.awt.Font
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.session.given
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}

case class UiPreset(
    name: String,
    config: AppConfig,
    themeName: String,
    pinnedPanels: List[UiPreset.PinnedPanel],
    targetEditorPaneCount: Option[Int] = None
)

object UiPreset:

  val builtIns: List[UiPreset] =
    List(writingPreset, documentationPreset, codePreset, reviewPreset)

  def builtInNames: List[String] =
    builtIns.map(_.name)

  def builtIn(name: String): Option[UiPreset] =
    builtIns.find(_.name.equalsIgnoreCase(name.trim))

  case class Preview(name: String, hint: String)

  object Preview:

    def fromPreset(preset: UiPreset): Preview =
      Preview(preset.name, previewHint(preset))

    def fromName(name: String): Preview =
      Preview(name.trim, "Saved workspace setup")

  private def previewHint(preset: UiPreset): String =
    List(
      Some(documentModeSummary(preset.config)),
      Option(preset.themeName).filter(_.nonEmpty),
      Some(s"${preset.config.motionPreset.configKey} motion"),
      Some(s"${textRevealSummary(preset.config.editorInsertionTransitionKind)} text reveal"),
      Some(s"${preset.config.materialPreset.configKey} material"),
      Some(proseFontSummary(preset.config)),
      paneCountSummary(preset.targetEditorPaneCount),
      panelSummary(preset.pinnedPanels)
    ).flatten.mkString("; ")

  private def documentModeSummary(config: AppConfig): String =
    config.defaultDocumentMode match
      case DefaultDocumentMode.RichText =>
        "rich text default"
      case DefaultDocumentMode.Markdown =>
        config.markdownViewMode match
          case MarkdownViewMode.Source       => "markdown source default"
          case MarkdownViewMode.SplitPreview => "markdown split preview"
          case MarkdownViewMode.InlineLens   => "markdown inline lens"
      case DefaultDocumentMode.PlainText =>
        "plain text default"

  private def proseFontSummary(config: AppConfig): String =
    s"${config.fontConfig.textFontFamily} ${formatPointSize(config.fontConfig.textFontSize)} prose"

  private def paneCountSummary(targetEditorPaneCount: Option[Int]): Option[String] =
    targetEditorPaneCount.collect {
      case 1     => "1 editor pane"
      case count => s"$count editor panes"
    }

  private def textRevealSummary(kind: TransitionKind): String =
    kind match
      case TransitionKind.Disabled               => "off"
      case TransitionKind.Fade                   => "fade"
      case TransitionKind.TypedText              => "typed"
      case TransitionKind.DirectionalSweep       => "directional"
      case TransitionKind.LineAndCharacterTandem => "tandem"
      case TransitionKind.OutlineThenContent     => "outline"

  private def formatPointSize(size: Float): String =
    if size == size.round.toFloat then size.toInt.toString + "pt"
    else f"$size%.1fpt"

  private def panelSummary(panels: List[PinnedPanel]): Option[String] =
    Option(panels.map(panel => s"${panel.position} ${panelContentName(panel.content)} ${panel.size}").mkString(", "))
      .filter(_.nonEmpty)

  private def panelContentName(content: PanelContentSnapshot): String =
    content match
      case PanelContentSnapshot.DirectoryTree(_, _, _) => "files"
      case PanelContentSnapshot.Terminal(_, _)         => "terminal"
      case PanelContentSnapshot.Outline(_)             => "outline"
      case PanelContentSnapshot.Diagnostics(_)         => "diagnostics"
      case PanelContentSnapshot.MarkdownPreview(_, _)  => "markdown preview"

  private def writingPreset: UiPreset =
    UiPreset(
      name = "Writing",
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withMotionPreset(MotionPreset.Subtle)
        .withEditorInsertionTransitionKind(TransitionKind.TypedText)
        .withMaterialPreset(MaterialPreset.Frosted)
        .withDefaultDocumentMode(DefaultDocumentMode.RichText)
        .withInterfaceDensity(InterfaceDensity.Spacious)
        .withTextAreaInsets(TextAreaInsets.fromPercent(22.0, 22.0))
        .copy(
          fontConfig = AppConfig.default.fontConfig.copy(
            textFontFamily = Font.SERIF,
            textFontSize = 18.0f,
            uiFontSize = 13.0f
          ),
          cursorInfoBarMode = CursorInfoBarMode.Position
        ),
      themeName = Theme.dark.name,
      pinnedPanels = List(PinnedPanel(PanelPosition.Left, 28, PanelContentSnapshot.Outline(Nil))),
      targetEditorPaneCount = Some(1)
    )

  private def documentationPreset: UiPreset =
    UiPreset(
      name = "Documentation",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(false)
        .withMotionPreset(MotionPreset.Subtle)
        .withEditorInsertionTransitionKind(TransitionKind.LineAndCharacterTandem)
        .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
        .withDefaultDocumentMode(DefaultDocumentMode.Markdown)
        .copy(
          fontConfig = AppConfig.default.fontConfig.copy(
            textFontFamily = Font.SANS_SERIF,
            textFontSize = 14.0f,
            fontSize = 13.0f
          )
        ),
      themeName = Theme.dark.name,
      pinnedPanels = List(PinnedPanel(PanelPosition.Left, 30, PanelContentSnapshot.Outline(Nil))),
      targetEditorPaneCount = Some(1)
    )

  private def codePreset: UiPreset =
    UiPreset(
      name = "Code",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withMotionPreset(MotionPreset.Reduced)
        .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        .withInterfaceDensity(InterfaceDensity.Compact)
        .copy(syntaxHighlightingEnabled = true, fontConfig = FontConfig()),
      themeName = Theme.dark.name,
      pinnedPanels = List(
        PinnedPanel(
          PanelPosition.Left,
          32,
          PanelContentSnapshot.DirectoryTree(".", selectedPath = None, expandedPaths = Nil)
        )
      )
    )

  private def reviewPreset: UiPreset =
    UiPreset(
      name = "Review",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withMotionPreset(MotionPreset.Reduced)
        .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        .withInterfaceDensity(InterfaceDensity.Comfortable)
        .copy(cursorInfoBarMode = CursorInfoBarMode.Detailed),
      themeName = Theme.dark.name,
      pinnedPanels = List(
        PinnedPanel(PanelPosition.Left, 30, PanelContentSnapshot.Outline(Nil)),
        PinnedPanel(PanelPosition.Bottom, 10, PanelContentSnapshot.Diagnostics(Nil))
      )
    )

  case class PinnedPanel(
      position: PanelPosition,
      size: Int,
      content: PanelContentSnapshot
  ):

    def toUiSurface(id: SurfaceId): UiSurface =
      UiSurface(
        id = id,
        content = content.toSurfaceContent,
        presentation = SurfacePresentation.Pinned(position, size)
      )

  enum PanelContentSnapshot:
    case DirectoryTree(rootPath: String, selectedPath: Option[String], expandedPaths: List[String])
    case Terminal(buffer: String, cursor: Int)
    case Outline(symbols: List[Symbol])
    case Diagnostics(issues: List[Diagnostic])
    case MarkdownPreview(bufferId: Int, title: String)

    def toSurfaceContent: SurfaceContent =
      this match
        case DirectoryTree(rootPath, selectedPath, expandedPaths) =>
          SurfaceContent.DirectoryTree(
            DirectoryTreeData(
              rootPath = Path.of(rootPath),
              expandedPaths = expandedPaths.map(Path.of(_)).toSet,
              entries = Map.empty
            ),
            selectedPath.map(Path.of(_))
          )
        case Terminal(buffer, cursor) =>
          SurfaceContent.Terminal(buffer, cursor)
        case Outline(symbols) =>
          SurfaceContent.Outline(symbols)
        case Diagnostics(issues) =>
          SurfaceContent.Diagnostics(issues)
        case MarkdownPreview(bufferId, title) =>
          SurfaceContent.MarkdownPreview(BufferId(bufferId), title)

  object PinnedPanel:

    def fromSurface(surface: UiSurface): Option[PinnedPanel] =
      surface.presentation match
        case SurfacePresentation.Pinned(position, size) =>
          fromSurfaceContent(surface.content, position, size)
        case _ =>
          None

    def fromPanelContent(content: PanelContent, position: PanelPosition, size: Int): Option[PinnedPanel] =
      val surfaceContent = content match
        case PanelContent.DirectoryTree(tree, selectedPath) => SurfaceContent.DirectoryTree(tree, selectedPath)
        case PanelContent.Terminal(buffer, cursor)          => SurfaceContent.Terminal(buffer, cursor)
        case PanelContent.Outline(symbols, activeLocation)  => SurfaceContent.Outline(symbols, activeLocation)
        case PanelContent.Diagnostics(issues)               => SurfaceContent.Diagnostics(issues)
        case PanelContent.MarkdownPreview(bufferId, title)  => SurfaceContent.MarkdownPreview(bufferId, title)

      fromSurfaceContent(surfaceContent, position, size)

    private def fromSurfaceContent(
      content: SurfaceContent,
      position: PanelPosition,
      size: Int
    ): Option[PinnedPanel] =
      snapshot(content).map(PinnedPanel(position, size, _))

    private def snapshot(content: SurfaceContent): Option[PanelContentSnapshot] =
      content match
        case SurfaceContent.DirectoryTree(tree, selectedPath) =>
          Some(
            PanelContentSnapshot.DirectoryTree(
              rootPath = tree.rootPath.toString,
              selectedPath = selectedPath.map(_.toString),
              expandedPaths = tree.expandedPaths.toList.map(_.toString).sorted
            )
          )
        case SurfaceContent.Terminal(buffer, cursor) =>
          Some(PanelContentSnapshot.Terminal(buffer, cursor))
        case SurfaceContent.Outline(symbols, _) =>
          Some(PanelContentSnapshot.Outline(symbols))
        case SurfaceContent.Diagnostics(issues) =>
          Some(PanelContentSnapshot.Diagnostics(issues))
        case SurfaceContent.MarkdownPreview(bufferId, title) =>
          Some(PanelContentSnapshot.MarkdownPreview(bufferId.value, title))
        case _ =>
          None

  def capture(
    name: String,
    state: AppState,
    preferredWindowSize: Option[com.serenity.config.PreferredWindowSize]
  ): UiPreset =
    val normalizedName = name.trim
    UiPreset(
      name = normalizedName,
      config = state.config.copy(preferredWindowSize = preferredWindowSize.orElse(state.config.preferredWindowSize)),
      themeName = state.theme.name,
      pinnedPanels = state.pinnedSurfaces.flatMap(PinnedPanel.fromSurface),
      targetEditorPaneCount = Option(state.layout.editorPanes.size).filter(_ > 0)
    )

  def applyToState(preset: UiPreset, state: AppState, theme: Theme): AppState =
    val unpinnedSurfaces = state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Pinned(_, _) => false
        case _                                => true
    }

    val withoutPinnedFocus =
      state.focus match
        case Focus.Surface(surfaceId) if state.pinnedSurfaces.exists(_.id == surfaceId) =>
          state.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.focus)
        case _ =>
          state.focus

    val (stateWithPanels, restoredPanels) =
      preset.pinnedPanels.foldLeft((state.copy(uiSurfaces = unpinnedSurfaces), List.empty[UiSurface])) {
        case ((currentState, panels), panel) =>
          val (nextState, surfaceId) = currentState.allocateSurfaceId
          val surface                = panel.toUiSurface(surfaceId)
          (nextState, panels :+ surface)
      }

    val restoredState = stateWithPanels.copy(
      config = preset.config,
      theme = theme,
      uiSurfaces = unpinnedSurfaces ++ restoredPanels,
      focus = withoutPinnedFocus,
      surfaceAnimations =
        stateWithPanels.surfaceAnimations.filterNot((surfaceId, _) => state.pinnedSurfaces.exists(_.id == surfaceId))
    )

    applyEditorPaneTarget(restoredState, preset.targetEditorPaneCount)

  private def applyEditorPaneTarget(state: AppState, targetEditorPaneCount: Option[Int]): AppState =
    targetEditorPaneCount match
      case Some(count) if count > 0 => resizeEditorPanes(state, count)
      case _                        => state

  private def resizeEditorPanes(state: AppState, targetCount: Int): AppState =
    val activePaneId = state.layout.activeEditorPaneId
      .filter(state.layout.editorPanes.contains)
      .orElse(state.layout.orderedPaneIds.find(state.layout.editorPanes.contains))
      .getOrElse(PaneId(0))
    val existingPaneIds = activePaneId :: state.layout.orderedPaneIds.filter(paneId =>
      paneId != activePaneId && state.layout.editorPanes.contains(paneId)
    )
    val existingTargetPaneIds = existingPaneIds.take(targetCount)
    val missingPaneCount      = targetCount - existingTargetPaneIds.size
    val newPaneIds =
      LazyList
        .iterate(state.nextPaneId.value)(_ + 1)
        .map(PaneId.apply)
        .filterNot(state.layout.editorPanes.contains)
        .take(missingPaneCount)
        .toList
    val targetPaneIds = existingTargetPaneIds ++ newPaneIds
    val priorityBufferIds = (state.focusedBufferId.toList ++
      state.layout.editorPanes.get(activePaneId).flatMap(_.bufferId).toList).distinct
    val visibleBufferIds = priorityBufferIds ++ state.bufferOrder.filter(bufferId =>
      state.buffers.contains(bufferId) && !priorityBufferIds.contains(bufferId)
    )
    val resizedPanes = targetPaneIds.zipWithIndex.map { (paneId, index) =>
      val basePane = state.layout.editorPanes.getOrElse(paneId, EditorPane.empty(paneId))
      paneId -> basePane.copy(id = paneId, bufferId = visibleBufferIds.lift(index))
    }.toMap
    val nextActivePaneId = targetPaneIds.headOption
    val nextPaneId = PaneId(
      (state.nextPaneId.value :: targetPaneIds.map(_.value + 1)).max
    )
    val nextFocus = state.focus match
      case Focus.EditorPane(paneId) if targetPaneIds.contains(paneId) =>
        state.focus
      case Focus.Surface(surfaceId) if state.surfaceById(surfaceId).nonEmpty =>
        state.focus
      case _ =>
        nextActivePaneId.map(Focus.EditorPane.apply).getOrElse(state.focus)

    state.copy(
      layout = state.layout.copy(
        editorPanes = resizedPanes,
        activeEditorPaneId = nextActivePaneId,
        paneOrder = targetPaneIds
      ),
      focus = nextFocus,
      nextPaneId = nextPaneId
    )

  given Encoder[PanelPosition] = Encoder.encodeString.contramap(_.toString)

  given Decoder[PanelPosition] = Decoder.decodeString.emap {
    case "Left"   => Right(PanelPosition.Left)
    case "Right"  => Right(PanelPosition.Right)
    case "Bottom" => Right(PanelPosition.Bottom)
    case "Top"    => Right(PanelPosition.Top)
    case other    => Left(s"Unknown PanelPosition: $other")
  }

  given Encoder[SymbolKind] = Encoder.encodeString.contramap(_.toString)

  given Decoder[SymbolKind] = Decoder.decodeString.emap {
    case "Function" => Right(SymbolKind.Function)
    case "Class"    => Right(SymbolKind.Class)
    case "Method"   => Right(SymbolKind.Method)
    case "Variable" => Right(SymbolKind.Variable)
    case "Constant" => Right(SymbolKind.Constant)
    case "Heading"  => Right(SymbolKind.Heading)
    case "Bookmark" => Right(SymbolKind.Bookmark)
    case "Section"  => Right(SymbolKind.Section)
    case other      => Left(s"Unknown SymbolKind: $other")
  }

  given Encoder[DiagnosticSeverity] = Encoder.encodeString.contramap(_.toString)

  given Decoder[DiagnosticSeverity] = Decoder.decodeString.emap {
    case "Error"   => Right(DiagnosticSeverity.Error)
    case "Warning" => Right(DiagnosticSeverity.Warning)
    case "Info"    => Right(DiagnosticSeverity.Info)
    case "Hint"    => Right(DiagnosticSeverity.Hint)
    case other     => Left(s"Unknown DiagnosticSeverity: $other")
  }

  given Encoder[Location] = deriveEncoder
  given Decoder[Location] = deriveDecoder

  given Encoder[Symbol] = deriveEncoder
  given Decoder[Symbol] = deriveDecoder

  given Encoder[Diagnostic] = deriveEncoder
  given Decoder[Diagnostic] = deriveDecoder

  given Encoder[PanelContentSnapshot] = deriveEncoder
  given Decoder[PanelContentSnapshot] = deriveDecoder

  given Encoder[PinnedPanel] = deriveEncoder
  given Decoder[PinnedPanel] = deriveDecoder

  given Encoder[UiPreset] = deriveEncoder
  given Decoder[UiPreset] = deriveDecoder

case class UiPresetIndex(presets: List[UiPreset]):
  def upsert(preset: UiPreset): UiPresetIndex =
    copy(presets = presets.filterNot(_.name.equalsIgnoreCase(preset.name)) :+ preset)

  def delete(name: String): UiPresetIndex =
    copy(presets = presets.filterNot(_.name.equalsIgnoreCase(name.trim)))

  def rename(sourceName: String, targetName: String): UiPresetIndex =
    val normalizedTarget = targetName.trim
    find(sourceName)
      .filter(_ => normalizedTarget.nonEmpty)
      .map(preset => delete(sourceName).upsert(preset.copy(name = normalizedTarget)))
      .getOrElse(this)

  def duplicate(sourceName: String, targetName: String): UiPresetIndex =
    val normalizedTarget = targetName.trim
    find(sourceName)
      .filter(_ => normalizedTarget.nonEmpty)
      .map(preset => upsert(preset.copy(name = normalizedTarget)))
      .getOrElse(this)

  def find(name: String): Option[UiPreset] =
    presets.find(_.name.equalsIgnoreCase(name.trim))

  def names: List[String] =
    presets.map(_.name)

object UiPresetIndex:
  val empty: UiPresetIndex = UiPresetIndex(Nil)

  given Encoder[UiPresetIndex] = deriveEncoder
  given Decoder[UiPresetIndex] = deriveDecoder

class UiPresetStore private (path: Path):
  import UiPresetIndex.given

  def load(): IO[UiPresetIndex] =
    IO.blocking(Files.exists(path)).flatMap {
      case false => IO.pure(UiPresetIndex.empty)
      case true =>
        IO.blocking(Files.readString(path, StandardCharsets.UTF_8)).flatMap { json =>
          IO.fromEither(decode[UiPresetIndex](json))
        }
    }

  def save(index: UiPresetIndex): IO[Unit] =
    IO.blocking {
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.writeString(path, index.asJson.spaces2, StandardCharsets.UTF_8)
    }.void

  def upsert(preset: UiPreset): IO[Unit] =
    load().flatMap(index => save(index.upsert(preset)))

  def delete(name: String): IO[Unit] =
    load().flatMap(index => save(index.delete(name)))

  def rename(sourceName: String, targetName: String): IO[Unit] =
    load().flatMap(index => save(index.rename(sourceName, targetName)))

  def duplicate(sourceName: String, targetName: String): IO[Unit] =
    load().flatMap(index => save(index.duplicate(sourceName, targetName)))

  def find(name: String): IO[Option[UiPreset]] =
    load().map(_.find(name))

  def list(): IO[List[UiPreset]] =
    load().map(_.presets)

object UiPresetStore:
  val defaultPath: Path = Paths.get(System.getProperty("user.home"), ".serenity", "ui-presets.json")

  def apply(path: Path): UiPresetStore =
    new UiPresetStore(path)

  def default: UiPresetStore =
    UiPresetStore(defaultPath)
