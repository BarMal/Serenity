package com.serenity.ui.presets

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import com.serenity.config.AppConfig
import com.serenity.session.given
import com.serenity.state.models.*
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
    pinnedPanels: List[UiPreset.PinnedPanel]
)

object UiPreset:

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
        case PanelContent.Outline(symbols)                  => SurfaceContent.Outline(symbols)
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
        case SurfaceContent.Outline(symbols) =>
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
      pinnedPanels = state.pinnedSurfaces.flatMap(PinnedPanel.fromSurface)
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

    stateWithPanels.copy(
      config = preset.config,
      theme = theme,
      uiSurfaces = unpinnedSurfaces ++ restoredPanels,
      focus = withoutPinnedFocus,
      surfaceAnimations =
        stateWithPanels.surfaceAnimations.filterNot((surfaceId, _) => state.pinnedSurfaces.exists(_.id == surfaceId))
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
