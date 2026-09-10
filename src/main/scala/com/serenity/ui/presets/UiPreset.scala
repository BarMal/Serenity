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
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.io.AtomicFileWriter
import com.serenity.session.SessionLayout
import com.serenity.session.given
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.layout.given
import com.serenity.ui.theme.Theme
import io.circe.*
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.decode
import io.circe.syntax.*

final case class UiPreset(
    name: String,
    config: AppConfig,
    themeName: String,
    dockedPanels: List[SessionDockedPanel] = Nil,
    targetEditorPaneCount: Option[Int] = None,
    workspaceTree: Option[SessionWorkspaceNode] = None,
    maximizedWorkspaceNodeId: Option[String] = None,
    schemaVersion: Int = UiPreset.CurrentSchemaVersion,
    unknownFields: JsonObject = JsonObject.empty,
    configUnknownFields: JsonObject = JsonObject.empty
):

  /** Flat view of the persisted panel content, independent of workspace-tree topology. */
  def pinnedPanels: List[SessionPinnedPanel] = dockedPanels.map(_.panel)

object UiPreset:

  /** Schema version 2 adds workspace trees, docked panel snapshots, and maximised-node identity -- the same shape
    * session persistence uses (see [[com.serenity.session.SessionState.CurrentSchemaVersion]]). Version-1 presets
    * decode through the legacy `pinnedPanels` field, migrated into `dockedPanels` with synthesised surface identity and
    * no persisted tree; applying such a preset falls back to the same legacy topology session restore uses. Invalid
    * version-2 trees fall back the same way, preserving buffers and supported panel content.
    */
  val CurrentSchemaVersion: Int = 2

  def normalizedName(name: String): String =
    Normalizer.normalize(name.trim, Normalizer.Form.NFC)

  def nameKey(name: String): String =
    normalizedName(name).toLowerCase(Locale.ROOT)

  val builtIns: List[UiPreset] =
    List(writingPreset, documentationPreset, codePreset, compactPreset, reviewPreset)

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

  private def patchWorkflowChrome(
    base: AppConfig,
    source: AppConfig,
    includeWordWrap: Boolean = false,
    includeContextualToolbar: Boolean = false,
    includeTextAreaInsets: Boolean = false
  ): AppConfig =
    base.withSurfaceConfig(
      base.surfaceConfig.copy(
        showLineNumbers = source.surfaceConfig.showLineNumbers,
        showGutter = source.surfaceConfig.showGutter,
        showPaneHeaders = source.surfaceConfig.showPaneHeaders,
        wordWrapEnabled =
          if includeWordWrap then source.surfaceConfig.wordWrapEnabled else base.surfaceConfig.wordWrapEnabled,
        contextualToolbarEnabled =
          if includeContextualToolbar then source.surfaceConfig.contextualToolbarEnabled
          else base.surfaceConfig.contextualToolbarEnabled,
        textAreaInsets =
          if includeTextAreaInsets then source.surfaceConfig.textAreaInsets else base.surfaceConfig.textAreaInsets
      )
    )

  private def mergeBuiltInWorkflowConfig(base: AppConfig, preset: UiPreset): AppConfig =
    val source         = preset.config
    val withMotion     = patchMotionConfig(base, source)
    val withTypography = patchTypographyConfig(withMotion, source)

    nameKey(preset.name) match
      case "writing" =>
        val withChrome = patchWorkflowChrome(withTypography, source, includeTextAreaInsets = true)
        withChrome
          .withSurfaceConfig(
            withChrome.surfaceConfig.copy(
              blurRadius = source.surfaceConfig.blurRadius,
              backgroundStyle = source.surfaceConfig.backgroundStyle,
              materialPreset = source.surfaceConfig.materialPreset
            )
          )
          .withDocumentConfig(source.documentConfig)
          .withInterfaceConfig(base.interfaceConfig.copy(density = source.interfaceDensity))
          .withCursorConfig(base.cursorConfig.copy(infoBarSegments = source.cursorInfoBarSegments))
      case "documentation" =>
        patchWorkflowChrome(withTypography, source)
          .withDocumentConfig(source.documentConfig)
      case "code" =>
        patchWorkflowChrome(withTypography, source)
          .withInterfaceConfig(base.interfaceConfig.copy(density = source.interfaceDensity))
          .withSyntaxHighlighting(source.languageToolsConfig.syntaxHighlightingEnabled)
      case "compact" =>
        patchWorkflowChrome(
          withTypography,
          source,
          includeWordWrap = true,
          includeContextualToolbar = true
        )
          .withInterfaceConfig(base.interfaceConfig.copy(density = source.interfaceDensity))
          .withSyntaxHighlighting(source.languageToolsConfig.syntaxHighlightingEnabled)
      case "review" =>
        patchWorkflowChrome(withTypography, source)
          .withInterfaceConfig(base.interfaceConfig.copy(density = source.interfaceDensity))
          .withCursorConfig(base.cursorConfig.copy(infoBarSegments = source.cursorInfoBarSegments))
      case _ => base

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

  final case class Preview(name: String, hint: String)

  object Preview:

    def fromPreset(preset: UiPreset): Preview =
      Preview(preset.name, previewHint(preset))

    def fromName(name: String): Preview =
      Preview(name.trim, "Saved workspace setup")

  private def previewHint(preset: UiPreset): String =
    List(
      Some(documentModeSummary(preset.config)),
      Option(preset.themeName).filter(_.nonEmpty),
      Some(s"${preset.config.surfaceConfig.motionPreset.configKey} motion"),
      Some(s"${textRevealSummary(preset.config.surfaceConfig.editorInsertionTransitionKind)} text reveal"),
      Some(s"${preset.config.surfaceConfig.materialPreset.configKey} material"),
      Some(s"${backgroundStyleSummary(preset.config.surfaceConfig.backgroundStyle)} background"),
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
    s"${config.editorConfig.fontConfig.textFontFamily} ${formatPointSize(config.editorConfig.fontConfig.textFontSize)} prose"

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

  private def panelSummary(panels: List[SessionPinnedPanel]): Option[String] =
    Option(panels.map(panel => s"${panel.position} ${panelContentName(panel.content)} ${panel.size}").mkString(", "))
      .filter(_.nonEmpty)

  private def panelContentName(content: SessionPanelContent): String =
    content match
      case SessionPanelContent.DirectoryTree(_, _, _) => "files"
      case SessionPanelContent.Terminal(_, _)         => "terminal"
      case SessionPanelContent.Outline(_)             => "outline"
      case SessionPanelContent.Comments(_)            => "comments"
      case SessionPanelContent.Diagnostics(_)         => "diagnostics"
      case SessionPanelContent.MarkdownPreview(_, _)  => "markdown preview"
      case SessionPanelContent.CompanionSprite        => "companion sprite"

  private def writingPreset: UiPreset =
    UiPreset(
      name = "Writing",
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withPaneHeaders(false)
        .withMotionPreset(MotionPreset.Subtle)
        .withEditorInsertionTransitionKind(TransitionKind.TypedText)
        .withMaterialPreset(MaterialPreset.Frosted)
        .withDefaultDocumentMode(DefaultDocumentMode.RichText)
        .withInterfaceDensity(InterfaceDensity.Spacious)
        .withTextAreaInsets(TextAreaInsets.fromPercent(22.0, 22.0))
        .withFontConfig(
          AppConfig.default.editorConfig.fontConfig.copy(
            textFontFamily = Font.SERIF,
            textFontSize = 18.0f,
            uiFontSize = 13.0f
          )
        )
        .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position)),
      themeName = Theme.dark.name,
      targetEditorPaneCount = Some(1)
    )

  private def documentationPreset: UiPreset =
    UiPreset(
      name = "Documentation",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(false)
        .withPaneHeaders(false)
        .withMotionPreset(MotionPreset.Subtle)
        .withEditorInsertionTransitionKind(TransitionKind.LineAndCharacterTandem)
        .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
        .withDefaultDocumentMode(DefaultDocumentMode.Markdown)
        .withFontConfig(
          AppConfig.default.editorConfig.fontConfig.copy(
            textFontFamily = Font.SANS_SERIF,
            textFontSize = 14.0f,
            fontSize = 13.0f
          )
        ),
      themeName = Theme.dark.name,
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
        .withFontConfig(FontConfig()),
      themeName = Theme.dark.name,
      dockedPanels = List(
        SessionDockedPanel(
          "code-directory-tree",
          SessionPinnedPanel(
            PanelPosition.Left,
            32,
            SessionPanelContent.DirectoryTree(".", selectedPath = None, expandedPaths = Nil)
          )
        )
      )
    )

  private def compactPreset: UiPreset =
    UiPreset(
      name = "Compact",
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .withPaneHeaders(true)
        .withWordWrap(false)
        .withContextualToolbarEnabled(false)
        .withMotionPreset(MotionPreset.Reduced)
        .withEditorInsertionTransitionKind(TransitionKind.Disabled)
        .withInterfaceDensity(InterfaceDensity.Compact)
        .withSyntaxHighlighting(true)
        .withFontConfig(FontConfig()),
      themeName = Theme.dark.name,
      targetEditorPaneCount = Some(1)
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
        .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)),
      themeName = Theme.dark.name,
      dockedPanels = List(
        SessionDockedPanel(
          "review-outline",
          SessionPinnedPanel(PanelPosition.Left, 30, SessionPanelContent.Outline(Nil))
        ),
        SessionDockedPanel(
          "review-diagnostics",
          SessionPinnedPanel(PanelPosition.Bottom, 10, SessionPanelContent.Diagnostics(Nil))
        )
      )
    )

  def capture(
    name: String,
    state: AppState,
    preferredWindowSize: Option[com.serenity.config.PreferredWindowSize]
  ): UiPreset =
    val normalizedName = name.trim
    val dockedPanels   = SessionDockedPanel.captureFrom(state)
    val workspaceTree  = SessionWorkspaceNode.captureFrom(state, dockedPanels)
    UiPreset(
      name = normalizedName,
      config = state.persisted.config.withWindowConfig(
        state.persisted.config.windowConfig.copy(
          preferredSize = preferredWindowSize.orElse(state.persisted.config.preferredWindowSize)
        )
      ),
      themeName = state.persisted.theme.name,
      dockedPanels = dockedPanels,
      targetEditorPaneCount = Option(state.persisted.layout.editorPanes.size).filter(_ > 0),
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = SessionWorkspaceNode.captureMaximizedNodeId(state, workspaceTree)
    )

  def applyToState(preset: UiPreset, state: AppState, theme: Theme): AppState =
    applyToState(preset, state, theme, preset.config)

  /** Apply a built-in workflow without replacing unrelated persisted configuration. */
  def applyBuiltInWorkflowToState(preset: UiPreset, state: AppState, theme: Theme): AppState =
    applyToState(preset, state, theme, mergeBuiltInWorkflowConfig(state.persisted.config, preset))

  private def applyToState(preset: UiPreset, state: AppState, theme: Theme, config: AppConfig): AppState =
    val unpinnedSurfaces = state.runtime.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Docked => false
        case _                          => true
    }
    // The old docked panels are wholly replaced by `preset.dockedPanels` below -- prune them from the tree here
    // (issue #817: the sole record of docked placement) rather than leaving stale entries for a later pass to notice.
    val prunedIds = state.pinnedSurfaces.map(_.id).toSet
    val prunedTree = prunedIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, id) =>
      tree.flatMap(_.removeSurface(id)).orElse(tree)
    }

    val withoutPinnedFocus =
      state.persisted.focus match
        case Focus.Surface(surfaceId) if state.pinnedSurfaces.exists(_.id == surfaceId) =>
          state.persisted.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)
        case _ =>
          state.persisted.focus

    // Panels restore keyed by their own persisted surface id (not freshly allocated) so identity matches whatever
    // the persisted workspace tree's docked-surface nodes reference.
    val restoredPanels        = preset.dockedPanels.map(docked => docked.panel.toUiSurface(SurfaceId(docked.surfaceId)))
    val reservedNextSurfaceId = state.runtime.nextSurfaceId.max(SessionLayout.nextSurfaceId(restoredPanels))

    // A preset's own persisted `workspaceTree` (issue #820) carries real nested topology, so it takes priority when
    // it still validates against the panes/panels actually being applied. Otherwise redock each panel one at a time
    // onto the existing (pruned) tree at its saved position/size, preserving whatever editor-pane split structure was
    // already there -- the same behaviour presets had before they could persist a tree of their own. Only when there
    // is no existing tree to redock onto do we fall back to rebuilding the legacy pane-strip-plus-dock topology (the
    // same last-resort session restore uses), so applying a preset never leaves an invalid layout.
    val decodedTree = preset.workspaceTree
      .flatMap(SessionWorkspaceNode.toWorkspaceNode)
      .map(WorkspaceTree.apply)
      .filter(_.validationErrors(state.persisted.layout.editorPanes.keySet, restoredPanels.map(_.id).toSet).isEmpty)

    val redockedTree = preset.dockedPanels.zip(restoredPanels).foldLeft(prunedTree) {
      case (Some(tree), (docked, surface)) =>
        val (splitId, leafId) = tree.nextDockIds(surface.id)
        tree
          .dockSized(surface.id, docked.panel.position, splitId, leafId, docked.panel.size, state.runtime.viewportSize)
          .orElse(Some(tree))
      case (None, _) =>
        None
    }

    val fallbackTree = SessionDockedPanel.fallbackWorkspaceTree(
      state.persisted.layout.orderedPaneIds,
      preset.dockedPanels
    )

    val treeWithPanels = decodedTree.orElse(redockedTree).orElse(fallbackTree)

    val maximizedWorkspaceNodeId = preset.maximizedWorkspaceNodeId
      .map(WorkspaceNodeId.apply)
      .filter(nodeId => treeWithPanels.exists(_.surfaceIdForNode(nodeId).nonEmpty))

    val restoredState = state.copy(
      persisted = state.persisted.copy(
        config = config,
        theme = theme,
        focus = withoutPinnedFocus,
        layout = state.persisted.layout.copy(
          workspaceTree = treeWithPanels.orElse(state.persisted.layout.workspaceTree),
          maximizedWorkspaceNodeId = maximizedWorkspaceNodeId
        )
      ),
      runtime = state.runtime.copy(
        uiSurfaces = unpinnedSurfaces ++ restoredPanels,
        nextSurfaceId = reservedNextSurfaceId,
        surfaceAnimations =
          state.runtime.surfaceAnimations.filterNot((surfaceId, _) => state.pinnedSurfaces.exists(_.id == surfaceId))
      )
    )

    applyEditorPaneTarget(restoredState, preset.targetEditorPaneCount)

  private def applyEditorPaneTarget(state: AppState, targetEditorPaneCount: Option[Int]): AppState =
    targetEditorPaneCount match
      case Some(count) if count > 0 => resizeEditorPanes(state, count)
      case _                        => state

  private def resizeEditorPanes(state: AppState, targetCount: Int): AppState =
    val activePaneId = state.persisted.layout.activeEditorPaneId
      .filter(state.persisted.layout.editorPanes.contains)
      .orElse(state.persisted.layout.orderedPaneIds.find(state.persisted.layout.editorPanes.contains))
      .getOrElse(PaneId(0))
    val existingPaneIds = activePaneId :: state.persisted.layout.orderedPaneIds.filter(paneId =>
      paneId != activePaneId && state.persisted.layout.editorPanes.contains(paneId)
    )
    val existingTargetPaneIds = existingPaneIds.take(targetCount)
    val missingPaneCount      = targetCount - existingTargetPaneIds.size
    val newPaneIds =
      LazyList
        .iterate(state.runtime.nextPaneId.value)(_ + 1)
        .map(PaneId.apply)
        .filterNot(state.persisted.layout.editorPanes.contains)
        .take(missingPaneCount)
        .toList
    val targetPaneIds = existingTargetPaneIds ++ newPaneIds
    val priorityBufferIds = (state.focusedBufferId.toList ++
      state.persisted.layout.editorPanes.get(activePaneId).flatMap(_.bufferId).toList).distinct
    val visibleBufferIds = priorityBufferIds ++ state.persisted.bufferOrder.filter(bufferId =>
      state.persisted.buffers.contains(bufferId) && !priorityBufferIds.contains(bufferId)
    )
    val resizedPanes = targetPaneIds.zipWithIndex.map { (paneId, index) =>
      val basePane = state.persisted.layout.editorPanes.getOrElse(paneId, EditorPane.empty(paneId))
      paneId -> basePane.copy(id = paneId, bufferId = visibleBufferIds.lift(index))
    }.toMap
    val nextActivePaneId = targetPaneIds.headOption
    // state.runtime.nextPaneId.value is always present, so folding from it as the seed always yields the true max.
    val nextPaneId = PaneId(
      targetPaneIds.map(_.value + 1).foldLeft(state.runtime.nextPaneId.value)(_ max _)
    )
    val nextFocus = state.persisted.focus match
      case Focus.EditorPane(paneId) if targetPaneIds.contains(paneId) =>
        state.persisted.focus
      case Focus.Surface(surfaceId) if state.surfaceById(surfaceId).nonEmpty =>
        state.persisted.focus
      case _ =>
        nextActivePaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)

    val droppedPaneIds = state.persisted.layout.editorPanes.keySet -- targetPaneIds.toSet
    val treeWithNewPanes = newPaneIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, newPaneId) =>
      val anchorPaneId = tree.map(_.paneIds).getOrElse(Nil).lastOption.orElse(existingTargetPaneIds.lastOption)
      anchorPaneId match
        case Some(anchor) =>
          tree
            .flatMap(
              _.split(
                anchor,
                newPaneId,
                SplitAxis.Horizontal,
                WorkspaceNodeId(s"resize-split-${anchor.value}-${newPaneId.value}"),
                WorkspaceNodeId(s"editor-${newPaneId.value}")
              )
            )
            .orElse(Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${newPaneId.value}"), newPaneId))))
        case None =>
          Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${newPaneId.value}"), newPaneId)))
    }
    val finalTree =
      droppedPaneIds.foldLeft(treeWithNewPanes)((tree, droppedId) => tree.flatMap(_.remove(droppedId)).orElse(tree))

    state.copy(
      persisted = state.persisted.copy(
        layout = state.persisted.layout.copy(
          editorPanes = resizedPanes,
          activeEditorPaneId = nextActivePaneId,
          workspaceTree = finalTree
        ),
        focus = nextFocus
      ),
      runtime = state.runtime.copy(nextPaneId = nextPaneId)
    )

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

  private val KnownFields = Set(
    "name",
    "config",
    "themeName",
    "dockedPanels",
    "pinnedPanels",
    "targetEditorPaneCount",
    "workspaceTree",
    "maximizedWorkspaceNodeId",
    "schemaVersion"
  )

  /** Reads the current `dockedPanels` shape when present; otherwise migrates the legacy `pinnedPanels` shape (a flat
    * panel list with no stable surface identity) by synthesising one, matching schema-v1 session decode.
    */
  private def decodeDockedPanels(cursor: HCursor): Decoder.Result[List[SessionDockedPanel]] =
    cursor.downField("dockedPanels").focus match
      case Some(_) =>
        cursor.get[List[SessionDockedPanel]]("dockedPanels")
      case None =>
        cursor.getOrElse[List[SessionPinnedPanel]]("pinnedPanels")(Nil).map { legacyPanels =>
          legacyPanels.zipWithIndex.map { case (panel, index) => SessionDockedPanel(s"legacy-panel-$index", panel) }
        }

  given Decoder[UiPreset] = Decoder.instance { cursor =>
    for
      name                     <- cursor.get[String]("name")
      config                   <- cursor.get[AppConfig]("config")
      themeName                <- cursor.get[String]("themeName")
      dockedPanels             <- decodeDockedPanels(cursor)
      targetEditorPaneCount    <- cursor.get[Option[Int]]("targetEditorPaneCount")
      workspaceTree            <- cursor.getOrElse[Option[SessionWorkspaceNode]]("workspaceTree")(None)
      maximizedWorkspaceNodeId <- cursor.getOrElse[Option[String]]("maximizedWorkspaceNodeId")(None)
      schemaVersion            <- cursor.getOrElse[Int]("schemaVersion")(1)
      _ <- Either.cond(
        schemaVersion <= CurrentSchemaVersion,
        (),
        DecodingFailure(
          s"Unsupported UI preset schema version: $schemaVersion (current: $CurrentSchemaVersion)",
          cursor.history
        )
      )
    yield
      val unknown = cursor.value.asObject.fold(JsonObject.empty)(objectValue =>
        JsonObject.fromIterable(objectValue.toIterable.filterNot((key, _) => KnownFields.contains(key)))
      )
      val configUnknownFields = cursor.downField("config").focus.flatMap(_.asObject).fold(JsonObject.empty) { rawConfig =>
        unknownJsonFields(rawConfig, config.asJson.asObject.getOrElse(JsonObject.empty))
      }
      UiPreset(
        name,
        config,
        themeName,
        dockedPanels,
        targetEditorPaneCount,
        workspaceTree,
        maximizedWorkspaceNodeId,
        schemaVersion,
        unknown,
        configUnknownFields
      )
  }

final case class UiPresetIndex(presets: List[UiPreset], unknownFields: JsonObject = JsonObject.empty):

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
      AtomicFileWriter.writeString(path, validIndex.asJson.spaces2)
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

object UiPresetStore:
  val defaultPath: Path = Paths.get(System.getProperty("user.home"), ".serenity", "ui-presets.json")

  private val inProcessLocks = new ConcurrentHashMap[Path, ReentrantLock]()

  private def inProcessLock(path: Path): ReentrantLock =
    inProcessLocks.computeIfAbsent(path.toAbsolutePath.normalize, _ => new ReentrantLock())

  def apply(path: Path): UiPresetStore =
    new UiPresetStore(path)

  def default: UiPresetStore =
    UiPresetStore(defaultPath)
