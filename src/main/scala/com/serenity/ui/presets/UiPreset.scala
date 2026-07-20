package com.serenity.ui.presets

import java.awt.Font
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.text.Normalizer
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock

import cats.effect.IO
import com.serenity.animation.TransitionKind
import com.serenity.config.*
import com.serenity.session.given
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import io.circe.syntax.*

case class UiPreset(
    name: String,
    config: AppConfig,
    themeName: String,
    pinnedPanels: List[UiPreset.PinnedPanel],
    targetEditorPaneCount: Option[Int] = None,
    unknownFields: JsonObject = JsonObject.empty,
    configUnknownFields: JsonObject = JsonObject.empty
)

/** The authoritative, unsaved workspace edit session for a UI preset. */
case class UiPresetEditSession(
    id: String,
    draftName: String,
    sourceName: Option[String],
    sourceRevision: Option[String] = None,
    baseline: UiPreset,
    baselineTheme: Theme,
    dirty: Boolean = false,
    draft: UiPreset,
    baselineLayout: Option[Layout] = None,
    baselineFocus: Option[Focus] = None,
    baselineNextPaneId: Option[PaneId] = None
)

object UiPreset:

  def normalizedName(name: String): String =
    Normalizer.normalize(name.trim, Normalizer.Form.NFC)

  def nameKey(name: String): String =
    normalizedName(name).toLowerCase(Locale.ROOT)

  val builtIns: List[UiPreset] =
    List(writingPreset, documentationPreset, codePreset, reviewPreset)

  def builtInNames: List[String] =
    builtIns.map(_.name)

  def builtIn(name: String): Option[UiPreset] =
    builtIns.find(_.name.equalsIgnoreCase(name.trim))

  enum Patch:
    case Appearance(config: AppConfig, themeName: Option[String] = None)
    case DocumentDefaults(config: AppConfig)
    case LanguageTools(config: AppConfig)
    case Motion(config: AppConfig)
    case TextDisplay(config: AppConfig)
    case Typography(config: AppConfig)

    def applyTo(preset: UiPreset): UiPreset =
      this match
        case Appearance(config, themeName) =>
          preset.copy(
            config = patchAppearanceConfig(preset.config, config),
            themeName = themeName.getOrElse(preset.themeName)
          )
        case DocumentDefaults(config) =>
          preset.copy(config = patchDocumentDefaultsConfig(preset.config, config))
        case LanguageTools(config) =>
          preset.copy(config = patchLanguageToolsConfig(preset.config, config))
        case Motion(config) =>
          preset.copy(config = patchMotionConfig(preset.config, config))
        case TextDisplay(config) =>
          preset.copy(config = patchTextDisplayConfig(preset.config, config))
        case Typography(config) =>
          preset.copy(config = patchTypographyConfig(preset.config, config))

  private def patchAppearanceConfig(base: AppConfig, source: AppConfig): AppConfig =
    base
      .withSurfaceConfig(
        base.surfaceConfig.copy(
          blurRadius = source.surfaceConfig.blurRadius,
          backgroundStyle = source.surfaceConfig.backgroundStyle,
          materialPreset = source.surfaceConfig.materialPreset
        )
      )
      .withInterfaceConfig(source.interfaceConfig)
      .withCursorConfig(source.cursorConfig)

  private def patchDocumentDefaultsConfig(base: AppConfig, source: AppConfig): AppConfig =
    base.withDocumentConfig(source.documentConfig)

  private def patchLanguageToolsConfig(base: AppConfig, source: AppConfig): AppConfig =
    base.withLanguageToolsConfig(source.languageToolsConfig)

  private def patchMotionConfig(base: AppConfig, source: AppConfig): AppConfig =
    base
      .withEditorConfig(base.editorConfig.copy(characterAnimation = source.editorConfig.characterAnimation))
      .withSurfaceConfig(
        base.surfaceConfig.copy(
          motionPreset = source.surfaceConfig.motionPreset,
          elementTransitionSpeedScale = source.surfaceConfig.elementTransitionSpeedScale,
          editorTextTransitionSpeedScale = source.surfaceConfig.editorTextTransitionSpeedScale,
          commandRunnerTransitionSpeedScale = source.surfaceConfig.commandRunnerTransitionSpeedScale,
          uiTransitionSpeedScale = source.surfaceConfig.uiTransitionSpeedScale,
          cursorTransitionSpeedScale = source.surfaceConfig.cursorTransitionSpeedScale,
          commandRunnerAnimation = source.surfaceConfig.commandRunnerAnimation,
          uiAnimation = source.surfaceConfig.uiAnimation,
          editorInsertionTransitionKind = source.surfaceConfig.editorInsertionTransitionKind,
          commandRunnerTransitionKind = source.surfaceConfig.commandRunnerTransitionKind,
          panelOpenTransitionKind = source.surfaceConfig.panelOpenTransitionKind,
          panelCloseTransitionKind = source.surfaceConfig.panelCloseTransitionKind,
          motionConfiguration = source.surfaceConfig.motionConfiguration
        )
      )

  private def patchTextDisplayConfig(base: AppConfig, source: AppConfig): AppConfig =
    base.withSurfaceConfig(
      base.surfaceConfig.copy(
        showLineNumbers = source.surfaceConfig.showLineNumbers,
        showGutter = source.surfaceConfig.showGutter,
        wordWrapEnabled = source.surfaceConfig.wordWrapEnabled,
        textAreaInsets = source.surfaceConfig.textAreaInsets,
        viewportSizing = source.surfaceConfig.viewportSizing
      )
    )

  private def patchTypographyConfig(base: AppConfig, source: AppConfig): AppConfig =
    base.withEditorConfig(base.editorConfig.copy(fontConfig = source.editorConfig.fontConfig))

  private def unknownJsonFields(raw: JsonObject, known: JsonObject): JsonObject =
    JsonObject.fromIterable(
      raw.toIterable.flatMap {
        case (key, rawValue) =>
          known(key) match
            case None => Some(key -> rawValue)
            case Some(knownValue) =>
              (rawValue.asObject, knownValue.asObject) match
                case (Some(rawObject), Some(knownObject)) =>
                  val nestedUnknown = unknownJsonFields(rawObject, knownObject)
                  Option.when(nestedUnknown.nonEmpty)(key -> Json.fromJsonObject(nestedUnknown))
                case _ => None
      }
    )

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
      Some(s"${backgroundStyleSummary(preset.config.backgroundStyle)} background"),
      Some(s"${preset.config.interfaceDensity.configKey} density"),
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

  private def backgroundStyleSummary(style: BackgroundStyle): String =
    style match
      case BackgroundStyle.Solid       => "solid"
      case BackgroundStyle.Transparent => "transparent"
      case BackgroundStyle.Frosted     => "frosted"
      case BackgroundStyle.GlassLike   => "glass"

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
          )
        )
        .withCursorInfoBarMode(CursorInfoBarMode.Position),
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
        .withSyntaxHighlighting(true)
        .copy(fontConfig = FontConfig()),
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
        .withCursorInfoBarMode(CursorInfoBarMode.Detailed),
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
        case SurfaceContent.Diagnostics(issues, _) =>
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
      config = state.config.withWindowConfig(
        state.config.windowConfig.copy(preferredSize = preferredWindowSize.orElse(state.config.preferredWindowSize))
      ),
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

  private given rawUiPresetEncoder: Encoder.AsObject[UiPreset] = deriveEncoder

  given Encoder[UiPreset] = Encoder.AsObject.instance { preset =>
    val encodedConfig = Json
      .fromJsonObject(preset.configUnknownFields)
      .deepMerge(preset.config.asJson)
    val encodedPreset = rawUiPresetEncoder
      .encodeObject(preset)
      .remove("unknownFields")
      .remove("configUnknownFields")
      .add("config", encodedConfig)
    preset.unknownFields.deepMerge(encodedPreset)
  }

  given Decoder[UiPreset] = Decoder.instance { cursor =>
    for
      name                  <- cursor.get[String]("name")
      config                <- cursor.get[AppConfig]("config")
      themeName             <- cursor.get[String]("themeName")
      pinnedPanels          <- cursor.get[List[PinnedPanel]]("pinnedPanels")
      targetEditorPaneCount <- cursor.get[Option[Int]]("targetEditorPaneCount")
    yield
      val knownKeys = Set("name", "config", "themeName", "pinnedPanels", "targetEditorPaneCount")
      val unknown = cursor.value.asObject.fold(JsonObject.empty)(objectValue =>
        JsonObject.fromIterable(objectValue.toIterable.filterNot((key, _) => knownKeys.contains(key)))
      )
      val configUnknownFields = cursor.downField("config").focus.flatMap(_.asObject).fold(JsonObject.empty) { rawConfig =>
        unknownJsonFields(rawConfig, config.asJson.asObject.getOrElse(JsonObject.empty))
      }
      UiPreset(name, config, themeName, pinnedPanels, targetEditorPaneCount, unknown, configUnknownFields)
  }

case class UiPresetIndex(presets: List[UiPreset], unknownFields: JsonObject = JsonObject.empty):

  def upsert(preset: UiPreset): UiPresetIndex =
    val existing = find(preset.name)
    val preserved =
      preset.copy(
        unknownFields = existing.fold(preset.unknownFields)(_.unknownFields.deepMerge(preset.unknownFields)),
        configUnknownFields = existing.fold(preset.configUnknownFields)(existing =>
          Json
            .fromJsonObject(existing.configUnknownFields)
            .deepMerge(Json.fromJsonObject(preset.configUnknownFields))
            .asObject
            .getOrElse(JsonObject.empty)
        )
      )
    copy(presets = presets.filterNot(item => UiPreset.nameKey(item.name) == UiPreset.nameKey(preset.name)) :+ preserved)

  def delete(name: String): UiPresetIndex =
    copy(presets = presets.filterNot(existing => UiPreset.nameKey(existing.name) == UiPreset.nameKey(name)))

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
    presets.find(existing => UiPreset.nameKey(existing.name) == UiPreset.nameKey(name))

  def names: List[String] =
    presets.map(_.name)

object UiPresetIndex:
  val empty: UiPresetIndex = UiPresetIndex(Nil)

  private given rawUiPresetIndexEncoder: Encoder.AsObject[UiPresetIndex] = deriveEncoder

  given Encoder[UiPresetIndex] = Encoder.AsObject.instance { index =>
    index.unknownFields.deepMerge(rawUiPresetIndexEncoder.encodeObject(index).remove("unknownFields"))
  }

  given Decoder[UiPresetIndex] = Decoder.instance { cursor =>
    cursor.get[List[UiPreset]]("presets").map { presets =>
      val knownKeys = Set("presets")
      val unknown = cursor.value.asObject.fold(JsonObject.empty)(objectValue =>
        JsonObject.fromIterable(objectValue.toIterable.filterNot((key, _) => knownKeys.contains(key)))
      )
      UiPresetIndex(presets, unknown)
    }
  }

enum UiPresetStoreConflict(message: String) extends RuntimeException(message):
  case SourceChanged(name: String) extends UiPresetStoreConflict(s"Preset '$name' changed outside this draft")
  case SourceMissing(name: String)
      extends UiPresetStoreConflict(s"Preset '$name' was deleted or renamed outside this draft")

class UiPresetStore private (path: Path):
  import UiPresetIndex.given

  private val mutationLockPath = path.resolveSibling(s".${path.getFileName.toString}.lock")

  private def withExclusiveMutationLock[A](operation: IO[A]): IO[A] =
    val processLock = UiPresetStore.inProcessLock(mutationLockPath)
    IO.blocking(processLock.lock())
      .bracket { _ =>
        IO.blocking {
          Option(mutationLockPath.getParent).foreach(Files.createDirectories(_))
          FileChannel.open(mutationLockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        }.bracket { channel =>
          IO.blocking(channel.lock()).bracket(_ => operation)(fileLock => IO.blocking(fileLock.release()))
        }(channel => IO.blocking(channel.close()))
      }(_ => IO.blocking(processLock.unlock()))

  def load(): IO[UiPresetIndex] =
    IO.blocking(Files.exists(path)).flatMap {
      case false => IO.pure(UiPresetIndex.empty)
      case true =>
        IO.blocking(Files.readString(path, StandardCharsets.UTF_8)).flatMap { json =>
          IO.fromEither(decode[UiPresetIndex](json))
        }
    }

  private def saveUnlocked(index: UiPresetIndex): IO[Unit] =
    IO.fromEither(validateIndex(index)).flatMap { validIndex =>
      IO.blocking {
        Option(path.getParent).foreach(Files.createDirectories(_))
        val directory = Option(path.getParent).getOrElse(Paths.get("."))
        val temporary = Files.createTempFile(directory, s".${path.getFileName.toString}.", ".tmp")
        try
          Files.writeString(temporary, validIndex.asJson.spaces2, StandardCharsets.UTF_8)
          Files.move(temporary, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        finally Files.deleteIfExists(temporary): Unit
      }.void
    }

  def save(index: UiPresetIndex): IO[Unit] =
    withExclusiveMutationLock(saveUnlocked(index))

  def upsert(preset: UiPreset): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap(index =>
        IO.fromEither(validateForUpsert(preset, index)).flatMap(valid => saveUnlocked(index.upsert(valid)))
      )
    }

  /** Creates a new custom preset and rejects any existing normalized name. */
  def create(preset: UiPreset): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        IO.raiseWhen(index.find(preset.name).nonEmpty)(
          new IllegalArgumentException(s"Preset name '${preset.name}' already exists")
        ) >>
          IO.fromEither(validateForUpsert(preset, index))
            .flatMap(valid => saveUnlocked(index.copy(presets = index.presets :+ valid)))
      }
    }

  def delete(name: String): IO[Unit] =
    withExclusiveMutationLock(load().flatMap(index => saveUnlocked(index.delete(name))))

  def rename(sourceName: String, targetName: String): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        for
          source <- IO.fromOption(index.find(sourceName))(
            new IllegalArgumentException(s"Preset '$sourceName' does not exist")
          )
          _ <- IO.raiseWhen(index.find(targetName).exists(_ != source))(
            new IllegalArgumentException(s"Preset name '$targetName' already exists")
          )
          renamed <- IO.fromEither(
            validateForUpsert(
              source.copy(name = targetName),
              index.copy(presets = index.presets.filterNot(_ == source))
            )
          )
          _ <- saveUnlocked(index.copy(presets = index.presets.filterNot(_ == source) :+ renamed))
        yield ()
      }
    }

  def duplicate(sourceName: String, targetName: String): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        for
          source <- IO.fromOption(index.find(sourceName))(
            new IllegalArgumentException(s"Preset '$sourceName' does not exist")
          )
          _ <- IO.raiseWhen(index.find(targetName).nonEmpty)(
            new IllegalArgumentException(s"Preset name '$targetName' already exists")
          )
          copy <- IO.fromEither(validateForUpsert(source.copy(name = targetName), index))
          _    <- saveUnlocked(index.copy(presets = index.presets :+ copy))
        yield ()
      }
    }

  def find(name: String): IO[Option[UiPreset]] =
    load().map(_.find(name))

  def list(): IO[List[UiPreset]] =
    load().map(_.presets)

  /** Replaces a custom preset only when its persisted source has not changed since the draft was opened. */
  def replace(sourceName: String, sourceRevision: String, replacement: UiPreset): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        index.find(sourceName) match
          case None => IO.raiseError(UiPresetStoreConflict.SourceMissing(sourceName))
          case Some(source) if revisionOf(source) != sourceRevision =>
            IO.raiseError(UiPresetStoreConflict.SourceChanged(sourceName))
          case Some(source) =>
            val withoutSource = index.copy(presets = index.presets.filterNot(_ == source))
            val preservingUnknownFields = replacement.copy(
              unknownFields = source.unknownFields.deepMerge(replacement.unknownFields),
              configUnknownFields = source.configUnknownFields.deepMerge(replacement.configUnknownFields)
            )
            IO.fromEither(validateForUpsert(preservingUnknownFields, withoutSource)).flatMap { valid =>
              saveUnlocked(withoutSource.copy(presets = withoutSource.presets :+ valid))
            }
      }
    }

  private def validateForUpsert(preset: UiPreset, index: UiPresetIndex): Either[IllegalArgumentException, UiPreset] =
    val name = UiPreset.normalizedName(preset.name)
    Either
      .cond(
        name.nonEmpty && !name.exists(ch => ch == '/' || ch == '\\' || ch == 0) &&
          name != "." && name != ".." && UiPreset.builtIn(name).isEmpty,
        preset.copy(name = name),
        new IllegalArgumentException("Preset name must be a non-built-in, non-path-like name")
      )
      .flatMap { valid =>
        index.find(valid.name) match
          case Some(existing) if existing.name != preset.name =>
            Left(new IllegalArgumentException(s"Preset name collides with existing preset '${existing.name}'"))
          case _ => Right(valid)
      }

  private def validateIndex(index: UiPresetIndex): Either[IllegalArgumentException, UiPresetIndex] =
    index.presets
      .foldLeft[Either[IllegalArgumentException, List[UiPreset]]](Right(Nil)) { (validated, preset) =>
        validated.flatMap(accepted => validateForUpsert(preset, UiPresetIndex(accepted)).map(accepted :+ _))
      }
      .map(presets => UiPresetIndex(presets, index.unknownFields))

  def revisionOf(preset: UiPreset): String =
    preset.asJson.noSpaces

object UiPresetStore:
  val defaultPath: Path = Paths.get(System.getProperty("user.home"), ".serenity", "ui-presets.json")

  private val inProcessLocks = new ConcurrentHashMap[Path, ReentrantLock]()

  private def inProcessLock(path: Path): ReentrantLock =
    inProcessLocks.computeIfAbsent(path.toAbsolutePath.normalize, _ => new ReentrantLock())

  def revisionOf(preset: UiPreset): String =
    preset.asJson.noSpaces

  def apply(path: Path): UiPresetStore =
    new UiPresetStore(path)

  def default: UiPresetStore =
    UiPresetStore(defaultPath)
