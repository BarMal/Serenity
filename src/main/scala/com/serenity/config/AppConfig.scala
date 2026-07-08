package com.serenity.config

import java.awt.Color
import java.nio.file.{Files, Path, Paths}

import scala.util.control.NonFatal

import com.serenity.animation.*
import com.serenity.lsp.config.LspUserConfig
import com.serenity.ui.fonts.FontLoader.FontConfig

enum BackgroundStyle:
  case Solid
  case Transparent
  case Frosted
  case GlassLike

enum MaterialPreset(val configKey: String):
  case Solid   extends MaterialPreset("solid")
  case Clear   extends MaterialPreset("clear")
  case Frosted extends MaterialPreset("frosted")
  case Crystal extends MaterialPreset("crystal")
  case Custom  extends MaterialPreset("custom")

  def backgroundStyle: BackgroundStyle =
    this match
      case Solid   => BackgroundStyle.Solid
      case Clear   => BackgroundStyle.Transparent
      case Frosted => BackgroundStyle.Frosted
      case Crystal => BackgroundStyle.GlassLike
      case Custom  => BackgroundStyle.Frosted

  def blurRadius: Float =
    this match
      case Solid | Clear => 0.0f
      case Frosted       => 0.3f
      case Crystal       => 0.65f
      case Custom        => 0.3f

enum MotionPreset(val configKey: String):
  case Reduced    extends MotionPreset("reduced")
  case Subtle     extends MotionPreset("subtle")
  case Smooth     extends MotionPreset("smooth")
  case Expressive extends MotionPreset("expressive")
  case Custom     extends MotionPreset("custom")

  def animationConfig: Option[AnimationConfig] =
    this match
      case Reduced    => AnimationConfig.none
      case Subtle     => AnimationConfig.subtle
      case Smooth     => AnimationConfig.smooth
      case Expressive => AnimationConfig.quick
      case Custom     => AnimationConfig.smooth

  def elementTransitionSettings: ElementTransitionSettings =
    this match
      case Reduced    => ElementTransitionSettings.disabled
      case Subtle     => ElementTransitionSettings.subtle
      case Smooth     => ElementTransitionSettings.smooth
      case Expressive => ElementTransitionSettings.expressive
      case Custom     => ElementTransitionSettings.smooth

enum RenderFpsTarget(val configKey: String, val framesPerSecond: Int):
  case Fps30    extends RenderFpsTarget("30", 30)
  case Fps60    extends RenderFpsTarget("60", 60)
  case Fps90    extends RenderFpsTarget("90", 90)
  case Fps120   extends RenderFpsTarget("120", 120)
  case Uncapped extends RenderFpsTarget("uncapped", 300)

object RenderFpsTarget:

  def fromConfigKey(value: String): Option[RenderFpsTarget] =
    value.trim.toLowerCase match
      case "30" | "30fps" | "fps30"       => Some(Fps30)
      case "60" | "60fps" | "fps60"       => Some(Fps60)
      case "90" | "90fps" | "fps90"       => Some(Fps90)
      case "120" | "120fps" | "fps120"    => Some(Fps120)
      case "uncapped" | "max" | "maximum" => Some(Uncapped)
      case _                              => None

enum CursorMode:
  case Blink
  case Breathe

enum CursorInfoBarMode:
  case Off
  case Position
  case Detailed

  def configKey: String =
    this match
      case Off      => "off"
      case Position => "position"
      case Detailed => "detailed"

enum CursorInfoBarPlacement(val configKey: String):
  case Floating     extends CursorInfoBarPlacement("floating")
  case PinnedBottom extends CursorInfoBarPlacement("pinned-bottom")

enum WindowChromeMode:
  case Native
  case Custom

enum MarkdownViewMode:
  case Source
  case SplitPreview
  case InlineLens

/** Default document mode for newly-created empty buffers. */
enum DefaultDocumentMode(val configKey: String):
  case PlainText extends DefaultDocumentMode("plain-text")
  case Markdown  extends DefaultDocumentMode("markdown")
  case RichText  extends DefaultDocumentMode("rich-text")

enum InterfaceDensity:
  case Compact
  case Comfortable
  case Spacious

  def configKey: String =
    this match
      case Compact     => "compact"
      case Comfortable => "comfortable"
      case Spacious    => "spacious"

case class InterfaceDensityMetrics(
    editorSpacerPercentage: Double,
    gutterHeight: Int,
    overlayGapRows: Int,
    commandSurfaceMaxHeight: Int,
    commandSurfaceMinHeight: Int,
    commandSurfaceVerticalPadding: Int
)

object InterfaceDensityMetrics:

  def forDensity(density: InterfaceDensity): InterfaceDensityMetrics =
    density match
      case InterfaceDensity.Compact =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.08,
          gutterHeight = 1,
          overlayGapRows = 0,
          commandSurfaceMaxHeight = 6,
          commandSurfaceMinHeight = 3,
          commandSurfaceVerticalPadding = 2
        )
      case InterfaceDensity.Comfortable =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.15,
          gutterHeight = 1,
          overlayGapRows = 1,
          commandSurfaceMaxHeight = 8,
          commandSurfaceMinHeight = 4,
          commandSurfaceVerticalPadding = 3
        )
      case InterfaceDensity.Spacious =>
        InterfaceDensityMetrics(
          editorSpacerPercentage = 0.22,
          gutterHeight = 2,
          overlayGapRows = 2,
          commandSurfaceMaxHeight = 10,
          commandSurfaceMinHeight = 5,
          commandSurfaceVerticalPadding = 4
        )

case class SpellCheckDictionaryFingerprint(
    path: String,
    exists: Boolean,
    isDirectory: Boolean,
    size: Long,
    lastModifiedMillis: Long
)

object SpellCheckDictionaryFingerprint:

  def fromPath(path: Path): SpellCheckDictionaryFingerprint =
    try
      val exists      = Files.exists(path)
      val isDirectory = exists && Files.isDirectory(path)
      SpellCheckDictionaryFingerprint(
        path = path.toAbsolutePath.normalize().toString,
        exists = exists,
        isDirectory = isDirectory,
        size = if exists && !isDirectory then Files.size(path) else 0L,
        lastModifiedMillis = if exists then Files.getLastModifiedTime(path).toMillis else 0L
      )
    catch
      case NonFatal(_) =>
        SpellCheckDictionaryFingerprint(
          path = path.toString,
          exists = false,
          isDirectory = false,
          size = 0L,
          lastModifiedMillis = 0L
        )

case class SpellCheckConfig(
    enabled: Boolean = false,
    languages: List[String] = List("en"),
    dictionaryPaths: List[String] = Nil,
    additionalWords: List[String] = Nil
):

  def normalized: SpellCheckConfig =
    copy(
      languages = languages.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct,
      dictionaryPaths = dictionaryPaths.map(_.trim).filter(_.nonEmpty).distinct,
      additionalWords = additionalWords.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
    )

  def dictionarySourcePaths: List[Path] =
    val config = normalized
    config.dictionaryPaths.flatMap(path => SpellCheckConfig.expandDictionaryPath(path, config.languages)).distinct

  def dictionaryFingerprints: List[SpellCheckDictionaryFingerprint] =
    SpellCheckConfig.dictionaryDependencyPaths(dictionarySourcePaths).map(SpellCheckDictionaryFingerprint.fromPath)

object SpellCheckConfig:

  def dictionaryDependencyPaths(dictionarySourcePaths: List[Path]): List[Path] =
    dictionarySourcePaths.flatMap(path => path :: affixPathForDictionary(path).toList).distinct

  def affixPathForDictionary(path: Path): Option[Path] =
    val fileName = path.getFileName
    Option(fileName).flatMap { name =>
      val text = name.toString
      if text.toLowerCase.endsWith(".dic") then Some(path.resolveSibling(text.dropRight(4) + ".aff"))
      else None
    }

  private def expandDictionaryPath(path: String, languages: List[String]): List[Path] =
    pathOption(path)
      .map { sourcePath =>
        if Files.isDirectory(sourcePath) then
          val candidates = languages.flatMap(languageCandidates).map(sourcePath.resolve)
          val existing   = candidates.filter(path => Files.exists(path))
          if existing.nonEmpty then existing else List(sourcePath)
        else
          val normalizedPath = sourcePath.toString
          if normalizedPath.toLowerCase.endsWith(".aff") then
            List(sourcePath.resolveSibling(sourcePath.getFileName.toString.dropRight(4) + ".dic"))
          else List(sourcePath)
      }
      .getOrElse(Nil)

  private def languageCandidates(language: String): List[String] =
    val normalized = language.trim.toLowerCase
    List(
      normalized,
      normalized.replace("-", "_"),
      normalized.replace("_", "-")
    ).distinct.map(_ + ".dic")

  private def pathOption(path: String): Option[Path] =
    try Some(Paths.get(path))
    catch case NonFatal(_) => None

case class PreferredWindowSize(width: Int, height: Int):
  def normalized: PreferredWindowSize =
    PreferredWindowSize(width.max(400), height.max(300))

case class CursorColorConfig(
    active: Option[Color] = None,
    inactive: Option[Color] = None
):
  def activeOr(default: Color): Color =
    active.getOrElse(default)

  def inactiveOr(activeColor: Color): Color =
    inactive.getOrElse(activeColor)

case class TextAreaInsets(
    left: Double = TextAreaInsets.DefaultInset,
    right: Double = TextAreaInsets.DefaultInset,
    top: Double = TextAreaInsets.DefaultInset,
    bottom: Double = TextAreaInsets.DefaultInset
):

  def normalized: TextAreaInsets =
    val normalizedLeft   = TextAreaInsets.clamp(left)
    val normalizedRight  = TextAreaInsets.clamp(right)
    val normalizedTop    = TextAreaInsets.clamp(top)
    val normalizedBottom = TextAreaInsets.clamp(bottom)
    val horizontalTotal  = normalizedLeft + normalizedRight
    val verticalTotal    = normalizedTop + normalizedBottom
    val horizontalScale =
      if horizontalTotal <= TextAreaInsets.MaxCombinedInset then 1.0
      else TextAreaInsets.MaxCombinedInset / horizontalTotal
    val verticalScale =
      if verticalTotal <= TextAreaInsets.MaxCombinedInset then 1.0
      else TextAreaInsets.MaxCombinedInset / verticalTotal
    copy(
      left = normalizedLeft * horizontalScale,
      right = normalizedRight * horizontalScale,
      top = normalizedTop * verticalScale,
      bottom = normalizedBottom * verticalScale
    )

  def leftPercent: Double =
    left * 100.0

  def rightPercent: Double =
    right * 100.0

  def topPercent: Double =
    top * 100.0

  def bottomPercent: Double =
    bottom * 100.0

object TextAreaInsets:
  val DefaultInset: Double     = 0.0
  val MaxInset: Double         = 0.45
  val MaxCombinedInset: Double = 0.8
  val MinTextAreaWidth: Double = 1.0 - MaxCombinedInset

  def fromPercent(
    left: Double,
    right: Double,
    top: Double = DefaultInset,
    bottom: Double = DefaultInset
  ): TextAreaInsets =
    TextAreaInsets(left / 100.0, right / 100.0, top / 100.0, bottom / 100.0).normalized

  def clamp(value: Double): Double =
    value.max(0.0).min(MaxInset)

/** Relative plus optional bounded sizing for one viewport axis. */
case class ViewportAxisSizing(
    percent: Double = 1.0,
    maxCells: Option[Int] = None
):

  def normalized: ViewportAxisSizing =
    copy(
      percent = ViewportAxisSizing.clampPercent(percent),
      maxCells = maxCells.map(_.max(1))
    )

  def percentValue: Double =
    normalized.percent * 100.0

  def resolve(availableCells: Int): Int =
    val relativeCells = math.max(1, (availableCells.max(1) * normalized.percent).toInt)
    normalized.maxCells.fold(relativeCells)(maxCells => math.min(relativeCells, maxCells))

object ViewportAxisSizing:
  val MinPercent: Double = 0.01
  val MaxPercent: Double = 1.0

  def fromPercent(percent: Double, maxCells: Option[Int] = None): ViewportAxisSizing =
    ViewportAxisSizing(percent / 100.0, maxCells).normalized

  def clampPercent(value: Double): Double =
    value.max(MinPercent).min(MaxPercent)

/** Configurable editor viewport sizing within each pane's available text area. */
case class ViewportSizing(
    width: ViewportAxisSizing = ViewportAxisSizing(),
    height: ViewportAxisSizing = ViewportAxisSizing()
):

  def normalized: ViewportSizing =
    copy(width = width.normalized, height = height.normalized)

/** Global application configuration */
case class AppConfig(
    characterAnimation: Option[AnimationConfig] = AnimationConfig.none,
    syntaxHighlightingEnabled: Boolean = false,
    hotkeyConfig: HotkeyConfig = HotkeyConfig(),
    focusedKeymapConfig: FocusedKeymapConfig = FocusedKeymapConfig(),
    fontConfig: FontConfig = FontConfig(),
    minimumPaneWidth: Int = 50,
    showLineNumbers: Boolean = true,
    showGutter: Boolean = true,
    wordWrapEnabled: Boolean = true,
    focusedTextBodyEnabled: Boolean = false,
    contextualToolbarEnabled: Boolean = true,
    blurRadius: Float = 0.0f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    materialPreset: MaterialPreset = MaterialPreset.Frosted,
    motionPreset: MotionPreset = MotionPreset.Reduced,
    elementTransitionSpeedScale: Double = 1.0,
    editorTextTransitionSpeedScale: Option[Double] = None,
    commandRunnerTransitionSpeedScale: Option[Double] = None,
    uiTransitionSpeedScale: Option[Double] = None,
    cursorTransitionSpeedScale: Option[Double] = None,
    commandRunnerAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    uiAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    commandRunnerVisibleRows: Option[Int] = None,
    renderFpsTarget: RenderFpsTarget = RenderFpsTarget.Fps60,
    editorInsertionTransitionKind: TransitionKind = TransitionKind.Fade,
    commandRunnerTransitionKind: Option[TransitionKind] = None,
    panelOpenTransitionKind: Option[TransitionKind] = None,
    panelCloseTransitionKind: Option[TransitionKind] = None,
    cursorMode: CursorMode = CursorMode.Blink,
    cursorColors: CursorColorConfig = CursorColorConfig(),
    cursorInfoBarMode: CursorInfoBarMode = CursorInfoBarMode.Off,
    cursorInfoBarPlacement: CursorInfoBarPlacement = CursorInfoBarPlacement.Floating,
    windowChromeMode: WindowChromeMode = WindowChromeMode.Native,
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    defaultDocumentMode: DefaultDocumentMode = DefaultDocumentMode.PlainText,
    interfaceDensity: InterfaceDensity = InterfaceDensity.Comfortable,
    uiElementGap: Int = 0,
    uiCornerRadiusPx: Int = 8,
    uiOutlineThicknessPx: Int = 2,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing(),
    preferredWindowSize: Option[PreferredWindowSize] = None,
    lspUserConfig: LspUserConfig = LspUserConfig.empty,
    spellCheck: SpellCheckConfig = SpellCheckConfig()
):
  /** Create a new config with character animation enabled */
  def withCharacterAnimation(config: AnimationConfig): AppConfig =
    copy(characterAnimation = Some(config), motionPreset = MotionPreset.Custom)

  /** Create a new config with character animation disabled */
  def withoutCharacterAnimation: AppConfig =
    copy(
      characterAnimation = None,
      motionPreset = MotionPreset.Reduced,
      editorTextTransitionSpeedScale = None,
      commandRunnerTransitionSpeedScale = None,
      uiTransitionSpeedScale = None,
      cursorTransitionSpeedScale = None,
      commandRunnerAnimation = None,
      uiAnimation = None,
      commandRunnerTransitionKind = None
    )

  /** Create a new config with syntax highlighting toggled */
  def withSyntaxHighlighting(enabled: Boolean): AppConfig =
    copy(syntaxHighlightingEnabled = enabled)

  def withHotkeyConfig(config: HotkeyConfig): AppConfig =
    copy(hotkeyConfig = config)

  def withHotkeyOverride(action: HotkeyAction, binding: String): AppConfig =
    copy(hotkeyConfig = hotkeyConfig.withBinding(action, binding))

  def resetHotkeyOverride(action: HotkeyAction): AppConfig =
    copy(hotkeyConfig = hotkeyConfig.resetBinding(action))

  def withFocusedKeymapConfig(config: FocusedKeymapConfig): AppConfig =
    copy(focusedKeymapConfig = config)

  def withEditorKeyOverride(action: EditorKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withEditorBinding(action, binding))

  def resetEditorKeyOverride(action: EditorKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetEditorBinding(action))

  def withCommandRunnerKeyOverride(action: CommandRunnerKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withCommandRunnerBinding(action, binding))

  def resetCommandRunnerKeyOverride(action: CommandRunnerKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetCommandRunnerBinding(action))

  def withModalKeyOverride(action: ModalKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withModalBinding(action, binding))

  def resetModalKeyOverride(action: ModalKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetModalBinding(action))

  def withPanelKeyOverride(action: PanelKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withPanelBinding(action, binding))

  def resetPanelKeyOverride(action: PanelKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetPanelBinding(action))

  def withPeekKeyOverride(action: PeekKeyAction, binding: String): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.withPeekBinding(action, binding))

  def resetPeekKeyOverride(action: PeekKeyAction): AppConfig =
    copy(focusedKeymapConfig = focusedKeymapConfig.resetPeekBinding(action))

  /** Create a new config with font configuration */
  def withFontConfig(config: FontConfig): AppConfig =
    copy(fontConfig = config)

  /** Create a new config with minimum pane width setting */
  def withMinimumPaneWidth(width: Int): AppConfig =
    copy(minimumPaneWidth = math.max(1, width))

  /** Create a new config with line numbers toggled */
  def withLineNumbers(enabled: Boolean): AppConfig =
    copy(showLineNumbers = enabled)

  /** Create a new config with gutter toggled */
  def withGutter(enabled: Boolean): AppConfig =
    copy(showGutter = enabled)

  /** Create a new config with word wrapping toggled */
  def withWordWrap(enabled: Boolean): AppConfig =
    copy(wordWrapEnabled = enabled)

  def withFocusedTextBody(enabled: Boolean): AppConfig =
    copy(focusedTextBodyEnabled = enabled)

  def withContextualToolbarEnabled(enabled: Boolean): AppConfig =
    copy(contextualToolbarEnabled = enabled)

  def withBlurRadius(r: Float): AppConfig =
    copy(blurRadius = r.max(0.0f).min(1.0f), materialPreset = MaterialPreset.Custom)

  def withBackgroundStyle(style: BackgroundStyle): AppConfig =
    copy(backgroundStyle = style, materialPreset = MaterialPreset.Custom)

  def withMaterialPreset(preset: MaterialPreset): AppConfig =
    preset match
      case MaterialPreset.Custom =>
        copy(materialPreset = MaterialPreset.Custom)
      case _ =>
        copy(
          materialPreset = preset,
          backgroundStyle = preset.backgroundStyle,
          blurRadius = preset.blurRadius
        )

  def withMotionPreset(preset: MotionPreset): AppConfig =
    preset match
      case MotionPreset.Custom =>
        copy(motionPreset = MotionPreset.Custom)
      case _ =>
        copy(
          motionPreset = preset,
          characterAnimation = preset.animationConfig,
          commandRunnerAnimation = preset.animationConfig,
          uiAnimation = preset.animationConfig
        )

  /** Transition policy derived from the selected motion preset and UI speed scale. */
  def elementTransitionSettings: ElementTransitionSettings =
    val baseSettings = motionPreset.elementTransitionSettings
    if !baseSettings.enabled then baseSettings
    else
      val transitionOverrides =
        List(
          Some(TransitionScope.EditorInsertion -> editorInsertionTransitionKind),
          commandRunnerTransitionKind.map(TransitionScope.CommandRunner -> _),
          panelOpenTransitionKind.map(TransitionScope.PanelOpen -> _),
          panelCloseTransitionKind.map(TransitionScope.PanelClose -> _)
        ).flatten.toMap

      baseSettings.copy(
        speedScale = effectiveUiTransitionSpeedScale,
        overrides = baseSettings.overrides ++ transitionOverrides
      )

  def withElementTransitionSpeedScale(scale: Double): AppConfig =
    copy(elementTransitionSpeedScale = AppConfig.clampElementTransitionSpeedScale(scale))

  def withEditorTextTransitionSpeedScale(scale: Option[Double]): AppConfig =
    copy(editorTextTransitionSpeedScale = scale.map(AppConfig.clampElementTransitionSpeedScale))

  def withCommandRunnerTransitionSpeedScale(scale: Option[Double]): AppConfig =
    copy(commandRunnerTransitionSpeedScale = scale.map(AppConfig.clampElementTransitionSpeedScale))

  def withUiTransitionSpeedScale(scale: Option[Double]): AppConfig =
    copy(uiTransitionSpeedScale = scale.map(AppConfig.clampElementTransitionSpeedScale))

  def withCursorTransitionSpeedScale(scale: Option[Double]): AppConfig =
    copy(cursorTransitionSpeedScale = scale.map(AppConfig.clampElementTransitionSpeedScale))

  def withCommandRunnerAnimation(animation: Option[AnimationConfig]): AppConfig =
    copy(commandRunnerAnimation = animation)

  def withUiAnimation(animation: Option[AnimationConfig]): AppConfig =
    copy(uiAnimation = animation)

  def withCommandRunnerVisibleRows(rows: Option[Int]): AppConfig =
    copy(commandRunnerVisibleRows = rows.map(AppConfig.clampCommandRunnerVisibleRows))

  def withRenderFpsTarget(target: RenderFpsTarget): AppConfig =
    copy(renderFpsTarget = target)

  def effectiveEditorTextTransitionSpeedScale: Double =
    editorTextTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveCommandRunnerTransitionSpeedScale: Double =
    commandRunnerTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveUiTransitionSpeedScale: Double =
    uiTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveCursorTransitionSpeedScale: Double =
    cursorTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  /** Character insertion animation after applying the effective editor text motion speed. */
  def scaledCharacterAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(characterAnimation, effectiveEditorTextTransitionSpeedScale)

  /** Command runner animation after applying the effective command runner motion speed. */
  def scaledCommandRunnerAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(commandRunnerAnimation, effectiveCommandRunnerTransitionSpeedScale)

  /** General UI animation after applying the effective UI motion speed. */
  def scaledUiAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(uiAnimation, effectiveUiTransitionSpeedScale)

  def withEditorInsertionTransitionKind(kind: TransitionKind): AppConfig =
    copy(editorInsertionTransitionKind = kind)

  def withCommandRunnerTransitionKind(kind: Option[TransitionKind]): AppConfig =
    copy(commandRunnerTransitionKind = kind)

  def effectiveCommandRunnerTransitionKind: TransitionKind =
    commandRunnerTransitionKind.getOrElse(TransitionKind.Fade)

  def withPanelOpenTransitionKind(kind: Option[TransitionKind]): AppConfig =
    copy(panelOpenTransitionKind = kind)

  def withPanelCloseTransitionKind(kind: Option[TransitionKind]): AppConfig =
    copy(panelCloseTransitionKind = kind)

  def effectivePanelOpenTransitionKind: TransitionKind =
    panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent)

  def effectivePanelCloseTransitionKind: TransitionKind =
    panelCloseTransitionKind.getOrElse(TransitionKind.Fade)

  def withCursorMode(mode: CursorMode): AppConfig =
    copy(cursorMode = mode)

  def withCursorColors(colors: CursorColorConfig): AppConfig =
    copy(cursorColors = colors)

  def withCursorInfoBarMode(mode: CursorInfoBarMode): AppConfig =
    copy(cursorInfoBarMode = mode)

  def withCursorInfoBarPlacement(placement: CursorInfoBarPlacement): AppConfig =
    copy(cursorInfoBarPlacement = placement)

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    copy(windowChromeMode = mode)

  def withMarkdownViewMode(mode: MarkdownViewMode): AppConfig =
    copy(markdownViewMode = mode)

  /** Create a new config with the default mode used for new empty buffers. */
  def withDefaultDocumentMode(mode: DefaultDocumentMode): AppConfig =
    copy(defaultDocumentMode = mode)

  def withInterfaceDensity(density: InterfaceDensity): AppConfig =
    copy(interfaceDensity = density)

  def withUiElementGap(gap: Int): AppConfig =
    copy(uiElementGap = AppConfig.clampUiElementGap(gap))

  def withUiCornerRadiusPx(radius: Int): AppConfig =
    copy(uiCornerRadiusPx = AppConfig.clampUiCornerRadiusPx(radius))

  def withUiOutlineThicknessPx(thickness: Int): AppConfig =
    copy(uiOutlineThicknessPx = AppConfig.clampUiOutlineThicknessPx(thickness))

  def withTextAreaInsets(insets: TextAreaInsets): AppConfig =
    copy(textAreaInsets = insets.normalized)

  def withTextAreaLeftInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(left = value))

  def withTextAreaRightInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(right = value))

  def withTextAreaTopInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(top = value))

  def withTextAreaBottomInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(bottom = value))

  def withViewportSizing(sizing: ViewportSizing): AppConfig =
    copy(viewportSizing = sizing.normalized)

  def withViewportWidthSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(viewportSizing.copy(width = sizing))

  def withViewportHeightSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(viewportSizing.copy(height = sizing))

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    copy(preferredWindowSize = Some(size.normalized))

  def withLspUserConfig(config: LspUserConfig): AppConfig =
    copy(lspUserConfig = config)

  def withSpellCheck(config: SpellCheckConfig): AppConfig =
    copy(spellCheck = config.normalized)

object AppConfig:

  val MinElementTransitionSpeedScale: Double = 0.0
  val MaxElementTransitionSpeedScale: Double = 4.0
  val MinUiElementGap: Int                   = 0
  val MaxUiElementGap: Int                   = 8
  val MinUiCornerRadiusPx: Int               = 0
  val MaxUiCornerRadiusPx: Int               = 32
  val MinUiOutlineThicknessPx: Int           = 1
  val MaxUiOutlineThicknessPx: Int           = 8
  val MinCommandRunnerVisibleRows: Int       = 1
  val MaxCommandRunnerVisibleRows: Int       = 20

  def clampElementTransitionSpeedScale(scale: Double): Double =
    scale.max(MinElementTransitionSpeedScale).min(MaxElementTransitionSpeedScale)

  def clampUiElementGap(gap: Int): Int =
    gap.max(MinUiElementGap).min(MaxUiElementGap)

  def clampUiCornerRadiusPx(radius: Int): Int =
    radius.max(MinUiCornerRadiusPx).min(MaxUiCornerRadiusPx)

  def clampUiOutlineThicknessPx(thickness: Int): Int =
    thickness.max(MinUiOutlineThicknessPx).min(MaxUiOutlineThicknessPx)

  def clampCommandRunnerVisibleRows(rows: Int): Int =
    rows.max(MinCommandRunnerVisibleRows).min(MaxCommandRunnerVisibleRows)

  def scaledAnimation(animation: Option[AnimationConfig], speedScale: Double): Option[AnimationConfig] =
    animation.flatMap(_.scaledBy(clampElementTransitionSpeedScale(speedScale)))

  /** Default configuration with smooth animations and syntax highlighting disabled */
  val default: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.smooth,
    uiAnimation = AnimationConfig.smooth,
    syntaxHighlightingEnabled = false,
    blurRadius = 0.3f,
    backgroundStyle = BackgroundStyle.Frosted,
    motionPreset = MotionPreset.Smooth
  )

  /** Test configuration with visible animations enabled */
  val withTestAnimations: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.quick,
    uiAnimation = AnimationConfig.quick,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Expressive
  )

  /** Quick fade-in animation configuration */
  val withQuickAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.quick,
    uiAnimation = AnimationConfig.quick,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Expressive
  )

  /** Smooth fade-in animation configuration */
  val withSmoothAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.smooth,
    uiAnimation = AnimationConfig.smooth,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Smooth
  )

  /** Subtle fade-in animation configuration */
  val withSubtleAnimation: AppConfig = AppConfig(
    characterAnimation = AnimationConfig.subtle,
    uiAnimation = AnimationConfig.subtle,
    syntaxHighlightingEnabled = false,
    motionPreset = MotionPreset.Subtle
  )
