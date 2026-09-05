package com.serenity.config

import java.awt.Color
import java.nio.file.{Files, Path, Paths}

import scala.util.control.NonFatal

import com.serenity.animation.*
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.Event
import com.serenity.lsp.config.LspUserConfig
import com.serenity.state.models.SurfacePlacement
import com.serenity.ui.fonts.FontLoader.FontConfig

enum BackgroundStyle:
  case Solid
  case Transparent
  case Frosted
  case GlassLike

  def configKey: String =
    this match
      case Solid       => "solid"
      case Transparent => "transparent"
      case Frosted     => "frosted"
      case GlassLike   => "glass-like"

object BackgroundStyle:

  def fromConfigKey(value: String): Option[BackgroundStyle] =
    val normalized = value.trim.toLowerCase.replace("_", "-")
    BackgroundStyle.values.find(_.configKey == normalized)

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
      case Frosted       => 0.18f
      case Crystal       => 0.42f
      case Custom        => 0.18f

enum PostProcessingEffect(val configKey: String):
  case Off              extends PostProcessingEffect("off")
  case Scanlines        extends PostProcessingEffect("scanlines")
  case Glow             extends PostProcessingEffect("glow")
  case ScanlinesAndGlow extends PostProcessingEffect("scanlines-glow")

object PostProcessingEffect:

  def fromConfigKey(value: String): Option[PostProcessingEffect] =
    value.trim.toLowerCase match
      case "off" | "none" | "disabled"      => Some(PostProcessingEffect.Off)
      case "scanlines" | "scanline" | "crt" => Some(PostProcessingEffect.Scanlines)
      case "glow"                           => Some(PostProcessingEffect.Glow)
      case "scanlines-glow" | "scanlines+glow" | "scanlines,glow" | "glow,scanlines" =>
        Some(PostProcessingEffect.ScanlinesAndGlow)
      case _ => None

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

/** How much of the damage a reducer reports the renderer honours. `Rows` coarsens cell-level damage to whole rows,
  * matching today's paint path. `Cells` honours column ranges where it is safe -- monospaced buffers only, see
  * `Damage`'s doc comment -- and falls back to row granularity for proportional or ligature-shaped text.
  */
enum RenderDamageGranularity(val configKey: String):
  case Rows  extends RenderDamageGranularity("rows")
  case Cells extends RenderDamageGranularity("cells")

object RenderDamageGranularity:

  def fromConfigKey(value: String): Option[RenderDamageGranularity] =
    value.trim.toLowerCase match
      case "rows" | "row"   => Some(Rows)
      case "cells" | "cell" => Some(Cells)
      case _                => None

enum CursorMode(val configKey: String):
  case Blink   extends CursorMode("blink")
  case Breathe extends CursorMode("breathe")

object CursorMode:

  def fromConfigKey(value: String): Option[CursorMode] =
    value.trim.toLowerCase match
      case "blink"                 => Some(CursorMode.Blink)
      case "breathe" | "breathing" => Some(CursorMode.Breathe)
      case _                       => None

/** One piece of text the cursor info bar can show, in the order the user has chosen to include them. Replaces the old
  * fixed Off/Position/Detailed presets (#1261) with an ordered, independently toggleable list.
  *
  * WritingSpeed (words typed per minute) is deliberately not a segment here: computing it needs edit-timestamp tracking
  * that doesn't exist anywhere in the app yet, so it's out of scope for this change and left as a follow-up rather than
  * half-built.
  */
enum CursorInfoBarSegment(val configKey: String):
  case Title       extends CursorInfoBarSegment("title")
  case Position    extends CursorInfoBarSegment("position")
  case WordCount   extends CursorInfoBarSegment("word_count")
  case CharCount   extends CursorInfoBarSegment("char_count")
  case ReadingTime extends CursorInfoBarSegment("reading_time")

object CursorInfoBarSegment:

  def fromConfigKey(value: String): Option[CursorInfoBarSegment] =
    values.find(_.configKey == value.trim.toLowerCase)

  /** Parses `cursor.info_bar`'s value: a comma-separated segment list (`"position,title"`), or one of the retired
    * Off/Minimal/Detailed shorthands for config.conf files written before segments existed.
    */
  def parseList(value: String): Option[List[CursorInfoBarSegment]] =
    value.trim.toLowerCase match
      case "" | "off" | "false" | "disabled" => Some(Nil)
      case "minimal"                         => Some(List(Position))
      case "detailed" | "full"               => Some(List(Position, Title))
      case trimmed =>
        val parsed = trimmed.split(",").toList.map(_.trim).filter(_.nonEmpty).map(fromConfigKey)
        Option.when(parsed.nonEmpty && parsed.forall(_.isDefined))(parsed.flatten)

enum CursorInfoBarPlacement(val configKey: String):
  case Floating     extends CursorInfoBarPlacement("floating")
  case PinnedBottom extends CursorInfoBarPlacement("pinned-bottom")

object CursorInfoBarPlacement:

  def fromConfigKey(value: String): Option[CursorInfoBarPlacement] =
    value.trim.toLowerCase match
      case "floating" | "float" =>
        Some(CursorInfoBarPlacement.Floating)
      case "pinned-bottom" | "bottom" | "pinned" =>
        Some(CursorInfoBarPlacement.PinnedBottom)
      case _ =>
        None

/** Selects how a buffer's `DocumentComment`s become visible (#1222).
  *
  * `Floating`: comments stay hidden until a highlighted range is clicked, opening the existing above-cursor lens
  * read-only first (a further click on its body enters edit) -- fully implemented.
  *
  * `Margin`: every comment for the visible buffer would render persistently in a side margin, with click-to-navigate
  * vs. click-in-body-to-edit routing. Reserved for a follow-up (see #1222) -- the margin layout/rendering does not
  * exist yet, so selecting it currently only turns off the `Floating` click-to-open behaviour without replacing it.
  */
enum CommentDisplayMode:
  case Floating
  case Margin

  def configKey: String =
    this match
      case Floating => "floating"
      case Margin   => "margin"

object CommentDisplayMode:

  def fromConfigKey(value: String): Option[CommentDisplayMode] =
    value.trim.toLowerCase match
      case "floating" => Some(CommentDisplayMode.Floating)
      case "margin"   => Some(CommentDisplayMode.Margin)
      case _          => None

enum WindowChromeMode(val configKey: String):
  case Auto         extends WindowChromeMode("auto")
  case Native       extends WindowChromeMode("native")
  case NativeThemed extends WindowChromeMode("native-themed")
  case Custom       extends WindowChromeMode("custom")

object WindowChromeMode:

  def fromConfigKey(value: String): Option[WindowChromeMode] =
    value.trim.toLowerCase match
      case "auto" | "default"                                  => Some(WindowChromeMode.Auto)
      case "custom" | "themed" | "serenity"                    => Some(WindowChromeMode.Custom)
      case "native-themed" | "native_themed" | "system-themed" => Some(WindowChromeMode.NativeThemed)
      case "native" | "os" | "system"                          => Some(WindowChromeMode.Native)
      case _                                                   => None

enum MarkdownViewMode(val configKey: String):
  case Source       extends MarkdownViewMode("source")
  case SplitPreview extends MarkdownViewMode("split-preview")
  case InlineLens   extends MarkdownViewMode("inline-lens")

object MarkdownViewMode:

  def fromConfigKey(value: String): Option[MarkdownViewMode] =
    value.trim.toLowerCase match
      case "source"                                                => Some(MarkdownViewMode.Source)
      case "split-preview" | "split_preview" | "split" | "preview" => Some(MarkdownViewMode.SplitPreview)
      case "inline-lens" | "inline_lens" | "lens"                  => Some(MarkdownViewMode.InlineLens)
      case _                                                       => None

enum ToolbarDisplayMode:
  case IconOnly
  case TextOnly
  case IconAndText

  def configKey: String =
    this match
      case IconOnly    => "icon-only"
      case TextOnly    => "text-only"
      case IconAndText => "icon-and-text"

object ToolbarDisplayMode:

  def fromConfigKey(value: String): Option[ToolbarDisplayMode] =
    value.trim.toLowerCase match
      case "icon" | "icon-only" | "icons-only" =>
        Some(IconOnly)
      case "text" | "text-only" =>
        Some(TextOnly)
      case "icon-and-text" | "icons-and-text" | "both" =>
        Some(IconAndText)
      case _ =>
        None

/** Default document mode for newly-created empty buffers. */
enum DefaultDocumentMode(val configKey: String):
  case PlainText extends DefaultDocumentMode("plain-text")
  case Markdown  extends DefaultDocumentMode("markdown")
  case RichText  extends DefaultDocumentMode("rich-text")

object DefaultDocumentMode:

  def fromConfigKey(value: String): Option[DefaultDocumentMode] =
    value.trim.toLowerCase match
      case "plain-text" | "plaintext" | "plain" | "text" => Some(DefaultDocumentMode.PlainText)
      case "markdown" | "md"                             => Some(DefaultDocumentMode.Markdown)
      case "rich-text" | "richtext" | "rich" | "rtf"     => Some(DefaultDocumentMode.RichText)
      case _                                             => None

/** Whether the workspace is primarily code or prose. Gates code-only tooling (LSP connections, project
  * build/run/test/debug) and filters which settings are shown by default.
  */
enum AppMode(val configKey: String):
  case Code  extends AppMode("code")
  case Prose extends AppMode("prose")

object AppMode:

  def fromConfigKey(value: String): Option[AppMode] =
    value.trim.toLowerCase match
      case "code"              => Some(AppMode.Code)
      case "prose" | "writing" => Some(AppMode.Prose)
      case _                   => None

enum InterfaceDensity:
  case Compact
  case Comfortable
  case Spacious

  def configKey: String =
    this match
      case Compact     => "compact"
      case Comfortable => "comfortable"
      case Spacious    => "spacious"

object InterfaceDensity:

  def fromConfigKey(value: String): Option[InterfaceDensity] =
    value.trim.toLowerCase match
      case "compact"     => Some(InterfaceDensity.Compact)
      case "comfortable" => Some(InterfaceDensity.Comfortable)
      case "spacious"    => Some(InterfaceDensity.Spacious)
      case _             => None

final case class InterfaceDensityMetrics(
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
          gutterHeight = 1,
          overlayGapRows = 0,
          commandSurfaceMaxHeight = 6,
          commandSurfaceMinHeight = 3,
          commandSurfaceVerticalPadding = 2
        )
      case InterfaceDensity.Comfortable =>
        InterfaceDensityMetrics(
          gutterHeight = 1,
          overlayGapRows = 1,
          commandSurfaceMaxHeight = 10,
          commandSurfaceMinHeight = 6,
          commandSurfaceVerticalPadding = 3
        )
      case InterfaceDensity.Spacious =>
        InterfaceDensityMetrics(
          gutterHeight = 2,
          overlayGapRows = 2,
          commandSurfaceMaxHeight = 12,
          commandSurfaceMinHeight = 8,
          commandSurfaceVerticalPadding = 4
        )

final case class SpellCheckDictionaryFingerprint(
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

final case class SpellCheckConfig(
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

object SpellCheckConfig:

  /** Resolves the on-disk dictionary paths implied by `config`, expanding directories into per-language candidates.
    * This walks the filesystem (`Files.isDirectory`/`Files.exists`) and must only be called from an explicit
    * `IO.blocking` boundary -- never from a method that otherwise looks pure.
    */
  def discoverDictionarySourcePaths(config: SpellCheckConfig): List[Path] =
    val normalized = config.normalized
    normalized.dictionaryPaths.flatMap(path => expandDictionaryPath(path, normalized.languages)).distinct

  /** Discovers dictionary source paths and fingerprints their current on-disk state. Filesystem IO throughout -- call
    * only from `IO.blocking`. The resulting immutable list is what pure analysis and state-commit comparisons must be
    * given, rather than recomputing this themselves.
    */
  def discoverDictionaryFingerprints(config: SpellCheckConfig): List[SpellCheckDictionaryFingerprint] =
    dictionaryDependencyPaths(discoverDictionarySourcePaths(config)).map(SpellCheckDictionaryFingerprint.fromPath)

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

final case class LanguageToolsConfig(
    syntaxHighlightingEnabled: Boolean = false,
    lspUserConfig: LspUserConfig = LspUserConfig.empty,
    spellCheck: SpellCheckConfig = SpellCheckConfig()
):

  def normalized: LanguageToolsConfig =
    copy(spellCheck = spellCheck.normalized)

final case class PreferredWindowSize(width: Int, height: Int):
  def normalized: PreferredWindowSize =
    PreferredWindowSize(width.max(400), height.max(300))

final case class WindowConfig(
    chromeMode: WindowChromeMode = WindowChromeMode.Auto,
    preferredSize: Option[PreferredWindowSize] = None
):

  def normalized: WindowConfig =
    copy(preferredSize = preferredSize.map(_.normalized))

final case class CursorColorConfig(
    active: Option[Color] = None,
    inactive: Option[Color] = None
):
  def activeOr(default: Color): Color =
    active.getOrElse(default)

  def inactiveOr(activeColor: Color): Color =
    inactive.getOrElse(activeColor)

/** #1295: `None` (default) keeps the active theme's own panel colour for the cursor info bar, matching every other
  * floating panel; `Some` overrides just that one surface's foreground/background, independent of theme -- mirrors
  * [[CursorColorConfig]]'s active/inactive override shape.
  */
final case class CursorInfoBarColorConfig(
    foreground: Option[Color] = None,
    background: Option[Color] = None
):
  def foregroundOr(default: Color): Color =
    foreground.getOrElse(default)

  def backgroundOr(default: Color): Color =
    background.getOrElse(default)

final case class CursorConfig(
    mode: CursorMode = CursorMode.Blink,
    colors: CursorColorConfig = CursorColorConfig(),
    infoBarSegments: List[CursorInfoBarSegment] = Nil,
    infoBarPlacement: CursorInfoBarPlacement = CursorInfoBarPlacement.Floating,
    infoBarColors: CursorInfoBarColorConfig = CursorInfoBarColorConfig()
)

final case class EditorConfig(
    characterAnimation: Option[AnimationConfig] = AnimationConfig.none,
    fontConfig: FontConfig = FontConfig(),
    minimumPaneWidth: Int = 50
):

  def normalized: EditorConfig =
    copy(minimumPaneWidth = math.max(1, minimumPaneWidth))

final case class DocumentConfig(
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    defaultMode: DefaultDocumentMode = DefaultDocumentMode.PlainText
)

final case class AppModeConfig(
    mode: AppMode = AppMode.Code,
    showAllSettingsRegardlessOfMode: Boolean = false
)

final case class InterfaceConfig(
    density: InterfaceDensity = InterfaceDensity.Comfortable,
    elementGap: Double = 0.0,
    cornerRadiusPx: Int = 8,
    outlineThicknessPx: Int = 2
):

  def normalized: InterfaceConfig =
    copy(
      elementGap = AppConfig.clampUiElementGap(elementGap),
      cornerRadiusPx = AppConfig.clampUiCornerRadiusPx(cornerRadiusPx),
      outlineThicknessPx = AppConfig.clampUiOutlineThicknessPx(outlineThicknessPx)
    )

final case class InputConfig(
    hotkeyConfig: HotkeyConfig = HotkeyConfig(),
    focusedKeymapConfig: FocusedKeymapConfig = FocusedKeymapConfig(),
    // Lines a single wheel notch scrolls. Three is the platform convention (`java.awt.event.MouseWheelEvent`'s own
    // unit-scroll default, and what most terminals send per notch), but it is a matter of taste and pointing device.
    wheelScrollLines: Int = 3
):

  def normalized: InputConfig =
    copy(wheelScrollLines = AppConfig.clampWheelScrollLines(wheelScrollLines))

final case class TextAreaInsets(
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
final case class ViewportAxisSizing(
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
final case class ViewportSizing(
    width: ViewportAxisSizing = ViewportAxisSizing(),
    height: ViewportAxisSizing = ViewportAxisSizing()
):

  def normalized: ViewportSizing =
    copy(width = width.normalized, height = height.normalized)

final case class SurfaceConfig(
    showLineNumbers: Boolean = true,
    showGutter: Boolean = true,
    showPaneHeaders: Boolean = true,
    // Opt-in (like `cursorInfoBarMode`): the status bar's default text (position/language/file) is covered by
    // exact-string assertions elsewhere, so this ships off and callers turn it on explicitly (#1203).
    showWordCount: Boolean = false,
    // Opt-in like `showWordCount`/`cursorInfoBarMode`: existing click/mouse-hit-testing and comment-lens behaviour
    // is covered by exact-state assertions elsewhere, so this defaults to the smaller, non-disruptive mode and
    // callers opt into margin mode explicitly once it ships (#1222).
    commentDisplayMode: CommentDisplayMode = CommentDisplayMode.Floating,
    wordWrapEnabled: Boolean = true,
    // Whether Up/Down under word wrap follow visual rows (the wrapped screen line) rather than jumping straight to
    // the previous/next logical line. Independent of wordWrapEnabled itself: only takes effect while wrap is also on.
    visualLineCursorNavigation: Boolean = true,
    // Off by default (preserves `CursorViewport.adjustForCursor`'s existing behaviour exactly): the cursor's line is
    // recentred on every move, but never past the document's own end, so a viewport near the last line falls back to
    // showing as much real content as fits rather than centring. On, that end clamp is lifted -- the caret's line
    // stays at its centred row even while typing at the very end of the document, padding with blank rows below it
    // the way iA Writer/Ulysses-style typewriter scrolling does (#1204, #1293).
    typewriterScrollingEnabled: Boolean = false,
    focusedTextBodyEnabled: Boolean = false,
    contextualToolbarEnabled: Boolean = true,
    contextualToolbarDisplayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconAndText,
    blurRadius: Float = 0.18f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    materialPreset: MaterialPreset = MaterialPreset.Frosted,
    postProcessingEffect: PostProcessingEffect = PostProcessingEffect.Off,
    uiShadowsEnabled: Boolean = true,
    motionPreset: MotionPreset = MotionPreset.Reduced,
    elementTransitionSpeedScale: Double = 1.0,
    editorTextTransitionSpeedScale: Option[Double] = None,
    commandRunnerTransitionSpeedScale: Option[Double] = None,
    uiTransitionSpeedScale: Option[Double] = None,
    cursorTransitionSpeedScale: Option[Double] = None,
    commandRunnerAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    uiAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    commandRunnerVisibleRows: Option[Int] = None,
    commandRunnerItemGapRows: Double = 0.0,
    commandRunnerCursorGapRows: Option[Double] = None,
    // Opt-out (unlike `showWordCount`/`commentDisplayMode`): the persistent key-hint footer (issue #931, Stage 3) is
    // the discoverability fix the stage exists to deliver, so it ships on by default; callers who want the old
    // dynamic-footer-only behaviour turn it off explicitly.
    commandRunnerShowKeyHints: Boolean = true,
    // Experimental prototype (off by default, unlike `commandRunnerShowKeyHints`): holding or double-tapping a bare
    // modifier peeks/opens the command runner near the cursor line. Ships disabled -- this is a single-panel spike,
    // not the finished feature -- and callers opt in explicitly.
    commandRunnerCursorPeekEnabled: Boolean = false,
    // Configurable per-user given real risk of OS/WM collision with Super/Meta on Linux.
    commandRunnerCursorPeekModifier: Modifier = Modifier.Meta,
    // Hold-vs-double-tap threshold in milliseconds. Defaults to `ModifierTapDetector.WindowMillis` (200L) for
    // consistency with the codebase's existing bare-modifier double-tap window (`ctrl+ctrl`-style hotkeys) rather
    // than introducing a second magic number.
    commandRunnerCursorPeekTapWindowMillis: Long = 200L,
    commandRunnerCursorPeekPlacement: SurfacePlacement = SurfacePlacement.BelowCursor,
    renderFpsTarget: RenderFpsTarget = RenderFpsTarget.Fps60,
    renderDamageGranularity: RenderDamageGranularity = RenderDamageGranularity.Rows,
    editorInsertionTransitionKind: TransitionKind = TransitionKind.Fade,
    commandRunnerTransitionKind: Option[TransitionKind] = None,
    panelOpenTransitionKind: Option[TransitionKind] = None,
    panelCloseTransitionKind: Option[TransitionKind] = None,
    motionConfiguration: Option[MotionConfig] = None,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing(),
    // None (default) keeps the active theme's own panel alpha, matching every other floating panel. Some overrides
    // just the cursor info bar's background alpha, independent of theme -- see `TextOverlayRenderer`'s per-row
    // background colour, the one paint site this is scoped to.
    cursorInfoBarBackgroundAlpha: Option[Double] = None
):

  def normalized: SurfaceConfig =
    copy(
      blurRadius = blurRadius.max(0.0f).min(1.0f),
      elementTransitionSpeedScale = AppConfig.clampElementTransitionSpeedScale(elementTransitionSpeedScale),
      editorTextTransitionSpeedScale = editorTextTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      commandRunnerTransitionSpeedScale =
        commandRunnerTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      uiTransitionSpeedScale = uiTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      cursorTransitionSpeedScale = cursorTransitionSpeedScale.map(AppConfig.clampElementTransitionSpeedScale),
      motionConfiguration = motionConfiguration.map(_.normalized),
      commandRunnerVisibleRows = commandRunnerVisibleRows.map(AppConfig.clampCommandRunnerVisibleRows),
      commandRunnerItemGapRows = AppConfig.clampCommandRunnerItemGapRows(commandRunnerItemGapRows),
      commandRunnerCursorGapRows = commandRunnerCursorGapRows.map(AppConfig.clampCommandRunnerCursorGapRows),
      commandRunnerCursorPeekTapWindowMillis =
        AppConfig.clampCommandRunnerCursorPeekTapWindowMillis(commandRunnerCursorPeekTapWindowMillis),
      textAreaInsets = textAreaInsets.normalized,
      viewportSizing = viewportSizing.normalized,
      cursorInfoBarBackgroundAlpha = cursorInfoBarBackgroundAlpha.map(AppConfig.clampCursorInfoBarBackgroundAlpha)
    )

  /** Speed scales as the legacy fields alone describe them: a per-family override if there is one, otherwise the
    * element-wide scale.
    *
    * These are what [[MotionConfig.fromLegacy]] reads when it derives a hierarchy from a configuration that has none,
    * so they cannot themselves consult the hierarchy -- that is what the `effective*` accessors below are for, and
    * asking one of those here would be circular.
    */
  private[config] def legacyEditorTextTransitionSpeedScale: Double =
    editorTextTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  private[config] def legacyCommandRunnerTransitionSpeedScale: Double =
    commandRunnerTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  private[config] def legacyUiTransitionSpeedScale: Double =
    uiTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  private[config] def legacyCursorTransitionSpeedScale: Double =
    cursorTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  /** The speed scale configured for a family: an explicit per-family override if the user set one, otherwise whatever
    * the authoritative hierarchy holds, and the element-wide scale only when there is nothing else to go on. This is
    * the value the settings surface shows as current -- the runtime value the renderer plans with comes from
    * `elementTransitionSettings`, where a `Reduced` preset legitimately disables motion the user has scaled.
    *
    * The middle step is the one that was missing. A configuration loaded from a file carries its scales in the
    * hierarchy and its legacy fields at their defaults -- those `Option`s are not written, being a record of what was
    * set explicitly rather than settings in their own right -- so resolving from the legacy fields alone reported the
    * element-wide default for a family the file plainly gave a scale to, and the settings row showed the wrong number
    * after every restart.
    */
  private def configuredFamilySpeedScale(family: MotionFamily, explicit: Option[Double]): Double =
    explicit
      .orElse(motionConfiguration.map(_ => effectiveMotionConfiguration.family(family).speedScale))
      .getOrElse(elementTransitionSpeedScale)

  def effectiveEditorTextTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.EditorText, editorTextTransitionSpeedScale)

  def effectiveCommandRunnerTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.CommandSurfaces, commandRunnerTransitionSpeedScale)

  def effectiveUiTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.UiTransitions, uiTransitionSpeedScale)

  def effectiveCursorTransitionSpeedScale: Double =
    configuredFamilySpeedScale(MotionFamily.Cursor, cursorTransitionSpeedScale)

  /** Resolve every runtime family from one hierarchy, preserving legacy fields when no hierarchy has been saved yet. */
  def effectiveMotionConfiguration: EffectiveMotionConfig =
    motionConfiguration match
      case Some(configuration) =>
        configuration.withFallback(MotionConfig.fromLegacy(this, configuration.baseline)).effective
      case None => MotionConfig.fromLegacy(this).effective

  def effectiveMotionBaseline: MotionPreset =
    motionConfiguration.fold(motionPreset)(_.baseline)

  def effectiveCommandRunnerTransitionKind: TransitionKind =
    motionConfiguration.fold(commandRunnerTransitionKind.getOrElse(TransitionKind.Fade))(_ =>
      effectiveMotionConfiguration.family(MotionFamily.CommandSurfaces).transitionKind
    )

  def effectivePanelOpenTransitionKind: TransitionKind =
    motionConfiguration.fold(panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent))(_ =>
      effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).transitionKindFor(TransitionScope.PanelOpen)
    )

  def effectivePanelCloseTransitionKind: TransitionKind =
    motionConfiguration.fold(panelCloseTransitionKind.getOrElse(TransitionKind.Fade))(_ =>
      effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).transitionKindFor(TransitionScope.PanelClose)
    )

  def elementTransitionSettings: ElementTransitionSettings =
    val uiMotion = effectiveMotionConfiguration.family(MotionFamily.UiTransitions)
    val baseSettings =
      if uiMotion.enabled then effectiveMotionBaseline.elementTransitionSettings else ElementTransitionSettings.disabled
    if !baseSettings.enabled then baseSettings
    else
      val transitionOverrides = motionConfiguration match
        case Some(_) =>
          List(
            TransitionScope.EditorInsertion -> effectiveMotionConfiguration
              .family(MotionFamily.EditorText)
              .transitionKind,
            TransitionScope.CommandRunner -> effectiveMotionConfiguration
              .family(MotionFamily.CommandSurfaces)
              .transitionKind,
            TransitionScope.PanelOpen -> effectiveMotionConfiguration
              .family(MotionFamily.PinnedPanels)
              .transitionKindFor(TransitionScope.PanelOpen),
            TransitionScope.PanelClose -> effectiveMotionConfiguration
              .family(MotionFamily.PinnedPanels)
              .transitionKindFor(TransitionScope.PanelClose)
          ).toMap
        case None =>
          List(
            Some(TransitionScope.EditorInsertion -> editorInsertionTransitionKind),
            commandRunnerTransitionKind.map(TransitionScope.CommandRunner -> _),
            panelOpenTransitionKind.map(TransitionScope.PanelOpen -> _),
            panelCloseTransitionKind.map(TransitionScope.PanelClose -> _)
          ).flatten.toMap

      baseSettings.copy(
        speedScale = uiMotion.speedScale,
        overrides = baseSettings.overrides ++ transitionOverrides
      )

  def editorInsertionTransitionSettings: ElementTransitionSettings =
    val editorMotion = effectiveMotionConfiguration.family(MotionFamily.EditorText)
    val baseSettings =
      if editorMotion.enabled then effectiveMotionBaseline.elementTransitionSettings
      else ElementTransitionSettings.disabled
    if !baseSettings.enabled then baseSettings
    else
      baseSettings.copy(
        speedScale = editorMotion.speedScale,
        overrides = baseSettings.overrides ++ Map(TransitionScope.EditorInsertion -> editorMotion.transitionKind)
      )

  /** Transition policy for pinned panels, with independent family timing and reveal strategy. */
  def pinnedPanelTransitionSettings: ElementTransitionSettings =
    motionConfiguration match
      case None => elementTransitionSettings
      case Some(_) =>
        val panelMotion = effectiveMotionConfiguration.family(MotionFamily.PinnedPanels)
        val baseSettings =
          if panelMotion.enabled then effectiveMotionBaseline.elementTransitionSettings
          else ElementTransitionSettings.disabled
        if !baseSettings.enabled then baseSettings
        else
          val timing = panelMotion.animation.fold(baseSettings.baseTiming)(animation =>
            baseSettings.baseTiming.copy(durationMs = animation.durationMs, staggerMs = animation.tickRateMs)
          )
          baseSettings.copy(
            baseTiming = timing,
            speedScale = panelMotion.speedScale,
            overrides = baseSettings.overrides ++ Map(
              TransitionScope.PanelOpen  -> panelMotion.transitionKindFor(TransitionScope.PanelOpen),
              TransitionScope.PanelClose -> panelMotion.transitionKindFor(TransitionScope.PanelClose)
            )
          )

object SurfaceConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "ui.material",
      "material.preset",
      "ui.post_processing",
      "ui.shadows",
      "ui.motion.preset",
      "ui.motion",
      "motion.preset",
      "ui.motion.speed_scale",
      "motion.speed_scale",
      "ui.motion.editor_text.speed_scale",
      "ui.motion.editor.text.speed_scale",
      "ui.motion.command_runner.speed_scale",
      "ui.motion.command.runner.speed_scale",
      "ui.motion.ui.speed_scale",
      "ui.motion.ui_elements.speed_scale",
      "ui.motion.ui.elements.speed_scale",
      "ui.motion.cursor.speed_scale",
      "ui.motion.cursor_speed_scale",
      "ui.motion.cursor.speed.scale",
      "ui.motion.command_runner",
      "ui.motion.command.runner",
      "ui.motion.command_runner_reveal",
      "ui.motion.command.runner.reveal",
      "ui.motion.ui",
      "ui.motion.ui_elements",
      "ui.motion.ui.elements",
      "ui.motion.editor_text",
      "ui.motion.editor.text",
      "ui.motion.panel_open",
      "ui.motion.panel.open",
      "ui.motion.panel_close",
      "ui.motion.panel.close",
      "command_runner.visible_rows",
      "command.runner.visible.rows",
      "command_runner.item_gap_rows",
      "command.runner.item.gap.rows",
      "command_runner.cursor_gap_rows",
      "command.runner.cursor.gap.rows",
      "render.fps",
      "ui.render.fps",
      "render.damage_granularity",
      "render.damage.granularity",
      "display.cursor_info_bar_background_alpha",
      "display.cursor_info_bar.background_alpha",
      "display.word_wrap",
      "display.word.wrap",
      "display.visual_line_navigation",
      "display.visual.line.navigation",
      "ui.blur_radius",
      "ui.blur.radius",
      "ui.background_style",
      "ui.background.style",
      "display.line_numbers",
      "display.line.numbers",
      "display.gutter",
      "display.word_count",
      "display.word.count",
      "display.comments",
      "command_runner.show_key_hints",
      "command.runner.show.key.hints",
      "command_runner.cursor_peek.enabled",
      "command.runner.cursor.peek.enabled",
      "command_runner.cursor_peek",
      "command.runner.cursor.peek",
      "command_runner.cursor_peek.modifier",
      "command.runner.cursor.peek.modifier",
      "command_runner.cursor_peek.tap_window_ms",
      "command.runner.cursor.peek.tap.window.ms",
      "command_runner.cursor_peek.placement",
      "command.runner.cursor.peek.placement",
      "display.pane_headers",
      "display.pane.headers",
      "display.focused_text_body",
      "display.focused.text.body",
      "display.contextual_toolbar",
      "display.contextual.toolbar",
      "display.contextual_toolbar_mode",
      "display.contextual.toolbar.mode",
      "text_area.left.percent",
      "text.area.left.percent",
      "text_area.right.percent",
      "text.area.right.percent",
      "text_area.top.percent",
      "text.area.top.percent",
      "text_area.bottom.percent",
      "text.area.bottom.percent",
      "viewport.width.percent",
      "viewport.width.max",
      "viewport.height.percent",
      "viewport.height.max"
    ) ++ Set("ui.motion.accessibility") ++ MotionFamily.values.flatMap { family =>
      Set(
        "enabled",
        "transition",
        "animation",
        "animation.preset",
        "animation.duration_ms",
        "animation.steps",
        "speed_scale"
      ).map(field => s"ui.motion.family.${family.configKey}.$field")
    } ++ Set(
      "ui.motion.family.pinned_panels.open_transition",
      "ui.motion.family.pinned_panels.close_transition"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "ui_material"                              -> "ui.material",
      "material_preset"                          -> "material.preset",
      "ui_motion"                                -> "ui.motion",
      "motion_preset"                            -> "motion.preset",
      "ui_motion_speed_scale"                    -> "ui.motion.speed_scale",
      "motion_speed_scale"                       -> "motion.speed_scale",
      "ui_motion_editor_text_speed_scale"        -> "ui.motion.editor_text.speed_scale",
      "ui_motion_command_runner_speed_scale"     -> "ui.motion.command_runner.speed_scale",
      "ui_motion_ui_speed_scale"                 -> "ui.motion.ui.speed_scale",
      "ui_motion_cursor_speed_scale"             -> "ui.motion.cursor.speed_scale",
      "ui_motion_command_runner"                 -> "ui.motion.command_runner",
      "ui_motion_command_runner_reveal"          -> "ui.motion.command_runner_reveal",
      "ui_motion_ui"                             -> "ui.motion.ui",
      "ui_motion_editor_text"                    -> "ui.motion.editor_text",
      "ui_motion_panel_open"                     -> "ui.motion.panel_open",
      "ui_motion_panel_close"                    -> "ui.motion.panel_close",
      "command_runner_visible_rows"              -> "command_runner.visible_rows",
      "command_runner_item_gap_rows"             -> "command_runner.item_gap_rows",
      "command_runner_cursor_gap_rows"           -> "command_runner.cursor_gap_rows",
      "render_fps"                               -> "render.fps",
      "ui_render_fps"                            -> "ui.render.fps",
      "render_damage_granularity"                -> "render.damage_granularity",
      "display_cursor_info_bar_background_alpha" -> "display.cursor_info_bar_background_alpha",
      "display_word_wrap"                        -> "display.word_wrap",
      "display_visual_line_navigation"           -> "display.visual_line_navigation",
      "display_pane_headers"                     -> "display.pane_headers",
      "display_focused_text_body"                -> "display.focused_text_body",
      "display_contextual_toolbar"               -> "display.contextual_toolbar",
      "display_contextual_toolbar_mode"          -> "display.contextual_toolbar_mode",
      "text_area_left_percent"                   -> "text_area.left.percent",
      "text_area_right_percent"                  -> "text_area.right.percent",
      "text_area_top_percent"                    -> "text_area.top.percent",
      "text_area_bottom_percent"                 -> "text_area.bottom.percent",
      "viewport_width_percent"                   -> "viewport.width.percent",
      "viewport_width_max"                       -> "viewport.width.max",
      "viewport_height_percent"                  -> "viewport.height.percent",
      "viewport_height_max"                      -> "viewport.height.max"
    )

    val commandRunnerVisibleRowsKeys: Set[String] =
      Set("command_runner.visible_rows", "command.runner.visible.rows", "command_runner_visible_rows")

    val commandRunnerItemGapRowsKeys: Set[String] =
      Set("command_runner.item_gap_rows", "command.runner.item.gap.rows", "command_runner_item_gap_rows")

    val commandRunnerCursorGapRowsKeys: Set[String] =
      Set("command_runner.cursor_gap_rows", "command.runner.cursor.gap.rows", "command_runner_cursor_gap_rows")

    val renderFpsKeys: Set[String] = Set("render.fps", "render_fps", "ui.render.fps", "ui_render_fps")

    val renderDamageGranularityKeys: Set[String] =
      Set("render.damage_granularity", "render.damage.granularity", "render_damage_granularity")

    val materialPresetKeys: Set[String] = Set("ui.material", "ui_material", "material.preset", "material_preset")

    val postProcessingKeys: Set[String] = Set("ui.post_processing")

    val uiShadowsKeys: Set[String] = Set("ui.shadows", "ui_shadows")

    /** `ui.motion.preset` is the spelling written. `ui.motion` is a leaf on a path whose children (`ui.motion.family`,
      * `ui.motion.accessibility`) HOCON would resolve by dropping it, so it survived only by being written as a quoted
      * key; it stays readable for files that already have it.
      */
    val motionPresetKeys: Set[String] =
      Set("ui.motion.preset", "ui.motion", "ui_motion", "motion.preset", "motion_preset")
    val motionAccessibilityKeys: Set[String] = Set("ui.motion.accessibility")
    val motionFamilyPrefix                   = "ui.motion.family."

    val motionFamilyKeys: Set[String] = MotionFamily.values.flatMap { family =>
      Set(
        "enabled",
        "transition",
        "animation",
        "animation.preset",
        "animation.duration_ms",
        "animation.steps",
        "speed_scale"
      ).map(field => s"$motionFamilyPrefix${family.configKey}.$field")
    }.toSet ++ Set(
      s"${motionFamilyPrefix}pinned_panels.open_transition",
      s"${motionFamilyPrefix}pinned_panels.close_transition"
    )

    val elementTransitionSpeedScaleKeys: Set[String] =
      Set("ui.motion.speed_scale", "motion.speed_scale", "ui_motion_speed_scale", "motion_speed_scale")

    val editorTextTransitionSpeedScaleKeys: Set[String] =
      Set("ui.motion.editor_text.speed_scale", "ui.motion.editor.text.speed_scale", "ui_motion_editor_text_speed_scale")

    val commandRunnerTransitionSpeedScaleKeys: Set[String] =
      Set(
        "ui.motion.command_runner.speed_scale",
        "ui.motion.command.runner.speed_scale",
        "ui_motion_command_runner_speed_scale"
      )

    val uiTransitionSpeedScaleKeys: Set[String] =
      Set(
        "ui.motion.ui.speed_scale",
        "ui.motion.ui_elements.speed_scale",
        "ui.motion.ui.elements.speed_scale",
        "ui_motion_ui_speed_scale"
      )

    val cursorTransitionSpeedScaleKeys: Set[String] =
      Set(
        "ui.motion.cursor.speed_scale",
        "ui.motion.cursor_speed_scale",
        "ui.motion.cursor.speed.scale",
        "ui_motion_cursor_speed_scale"
      )

    val commandRunnerAnimationKeys: Set[String] =
      Set("ui.motion.command_runner", "ui.motion.command.runner", "ui_motion_command_runner")

    val commandRunnerTransitionKeys: Set[String] =
      Set("ui.motion.command_runner_reveal", "ui.motion.command.runner.reveal", "ui_motion_command_runner_reveal")

    val uiAnimationKeys: Set[String] =
      Set("ui.motion.ui", "ui.motion.ui_elements", "ui.motion.ui.elements", "ui_motion_ui")

    val editorTextTransitionKeys: Set[String] =
      Set("ui.motion.editor_text", "ui.motion.editor.text", "ui_motion_editor_text")

    val panelOpenTransitionKeys: Set[String] =
      Set("ui.motion.panel_open", "ui.motion.panel.open", "ui_motion_panel_open")

    val panelCloseTransitionKeys: Set[String] =
      Set("ui.motion.panel_close", "ui.motion.panel.close", "ui_motion_panel_close")

    val cursorInfoBarBackgroundAlphaKeys: Set[String] =
      Set(
        "display.cursor_info_bar_background_alpha",
        "display.cursor_info_bar.background_alpha",
        "display_cursor_info_bar_background_alpha"
      )

    val wordWrapKeys: Set[String] = Set("display.word_wrap", "display.word.wrap", "display_word_wrap")

    val visualLineNavigationKeys: Set[String] =
      Set("display.visual_line_navigation", "display.visual.line.navigation", "display_visual_line_navigation")

    val blurRadiusKeys: Set[String] = Set("ui.blur_radius", "ui.blur.radius", "ui_blur_radius")

    val backgroundStyleKeys: Set[String] = Set("ui.background_style", "ui.background.style", "ui_background_style")

    val lineNumberKeys: Set[String] =
      Set("display.line_numbers", "display.line.numbers", "display_line_numbers")

    val gutterKeys: Set[String] = Set("display.gutter", "display_gutter")

    val wordCountKeys: Set[String] = Set("display.word_count", "display.word.count", "display_word_count")

    val commentDisplayModeKeys: Set[String] = Set("display.comments", "display_comments")

    val commandRunnerShowKeyHintsKeys: Set[String] =
      Set("command_runner.show_key_hints", "command.runner.show.key.hints", "command_runner_show_key_hints")

    /** `.enabled` rather than `command_runner.cursor_peek` itself: the peek settings below are children of that path,
      * and HOCON drops a leaf that also has children.
      */
    val commandRunnerCursorPeekKeys: Set[String] =
      Set(
        "command_runner.cursor_peek.enabled",
        "command.runner.cursor.peek.enabled",
        "command_runner.cursor_peek",
        "command.runner.cursor.peek",
        "command_runner_cursor_peek"
      )

    val commandRunnerCursorPeekModifierKeys: Set[String] =
      Set("command_runner.cursor_peek.modifier", "command.runner.cursor.peek.modifier")

    val commandRunnerCursorPeekTapWindowKeys: Set[String] =
      Set("command_runner.cursor_peek.tap_window_ms", "command.runner.cursor.peek.tap.window.ms")

    val commandRunnerCursorPeekPlacementKeys: Set[String] =
      Set("command_runner.cursor_peek.placement", "command.runner.cursor.peek.placement")

    val paneHeaderKeys: Set[String] =
      Set("display.pane_headers", "display.pane.headers", "display_pane_headers")

    val focusedTextBodyKeys: Set[String] =
      Set("display.focused_text_body", "display.focused.text.body", "display_focused_text_body")

    val contextualToolbarKeys: Set[String] =
      Set("display.contextual_toolbar", "display.contextual.toolbar", "display_contextual_toolbar")

    val contextualToolbarModeKeys: Set[String] =
      Set(
        "display.contextual_toolbar_mode",
        "display.contextual.toolbar.mode",
        "display_contextual_toolbar_mode"
      )

    val textAreaLeftPercentKeys: Set[String] =
      Set("text_area.left.percent", "text.area.left.percent", "text_area_left_percent")

    val textAreaRightPercentKeys: Set[String] =
      Set("text_area.right.percent", "text.area.right.percent", "text_area_right_percent")

    val textAreaTopPercentKeys: Set[String] =
      Set("text_area.top.percent", "text.area.top.percent", "text_area_top_percent")

    val textAreaBottomPercentKeys: Set[String] =
      Set("text_area.bottom.percent", "text.area.bottom.percent", "text_area_bottom_percent")

    val viewportWidthPercentKeys: Set[String] = Set("viewport.width.percent", "viewport_width_percent")

    val viewportWidthMaxKeys: Set[String] = Set("viewport.width.max", "viewport_width_max")

    val viewportHeightPercentKeys: Set[String] = Set("viewport.height.percent", "viewport_height_percent")

    val viewportHeightMaxKeys: Set[String] = Set("viewport.height.max", "viewport_height_max")

    private val handledKeys: Set[String] =
      materialPresetKeys ++
        postProcessingKeys ++
        uiShadowsKeys ++
        motionPresetKeys ++
        motionAccessibilityKeys ++
        motionFamilyKeys ++
        elementTransitionSpeedScaleKeys ++
        editorTextTransitionSpeedScaleKeys ++
        commandRunnerTransitionSpeedScaleKeys ++
        uiTransitionSpeedScaleKeys ++
        cursorTransitionSpeedScaleKeys ++
        commandRunnerAnimationKeys ++
        commandRunnerTransitionKeys ++
        uiAnimationKeys ++
        editorTextTransitionKeys ++
        panelOpenTransitionKeys ++
        panelCloseTransitionKeys ++
        commandRunnerVisibleRowsKeys ++
        commandRunnerItemGapRowsKeys ++
        commandRunnerCursorGapRowsKeys ++
        cursorInfoBarBackgroundAlphaKeys ++
        renderFpsKeys ++
        renderDamageGranularityKeys ++
        wordWrapKeys ++
        blurRadiusKeys ++
        backgroundStyleKeys ++
        visualLineNavigationKeys ++
        lineNumberKeys ++
        gutterKeys ++
        wordCountKeys ++
        commentDisplayModeKeys ++
        commandRunnerShowKeyHintsKeys ++
        commandRunnerCursorPeekKeys ++
        commandRunnerCursorPeekModifierKeys ++
        commandRunnerCursorPeekTapWindowKeys ++
        commandRunnerCursorPeekPlacementKeys ++
        paneHeaderKeys ++
        focusedTextBodyKeys ++
        contextualToolbarKeys ++
        contextualToolbarModeKeys ++
        textAreaLeftPercentKeys ++
        textAreaRightPercentKeys ++
        textAreaTopPercentKeys ++
        textAreaBottomPercentKeys ++
        viewportWidthPercentKeys ++
        viewportWidthMaxKeys ++
        viewportHeightPercentKeys ++
        viewportHeightMaxKeys

    def handles(key: String): Boolean =
      handledKeys.contains(key)

    def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val trimmed = value.trim
      if materialPresetKeys.contains(key) then parseMaterialPreset(trimmed).map(config.withMaterialPreset)
      else if postProcessingKeys.contains(key) then
        PostProcessingEffect.fromConfigKey(trimmed).map(config.withPostProcessingEffect)
      else if uiShadowsKeys.contains(key) then trimmed.toBooleanOption.map(config.withUiShadowsEnabled)
      else if motionPresetKeys.contains(key) then parseMotionPreset(trimmed).map(config.withMotionPreset)
      else if motionAccessibilityKeys.contains(key) then
        MotionAccessibility.fromConfigKey(trimmed).map(config.withMotionAccessibility)
      else if motionFamilyKeys.contains(key) then parseMotionFamily(config, key, trimmed)
      else if elementTransitionSpeedScaleKeys.contains(key) then
        parseElementTransitionSpeedScale(trimmed).map(config.withElementTransitionSpeedScale)
      else if editorTextTransitionSpeedScaleKeys.contains(key) then
        parseElementTransitionSpeedScale(trimmed).map(scale => config.withEditorTextTransitionSpeedScale(Some(scale)))
      else if commandRunnerTransitionSpeedScaleKeys.contains(key) then
        parseElementTransitionSpeedScale(trimmed).map(scale =>
          config.withCommandRunnerTransitionSpeedScale(Some(scale))
        )
      else if uiTransitionSpeedScaleKeys.contains(key) then
        parseElementTransitionSpeedScale(trimmed).map(scale => config.withUiTransitionSpeedScale(Some(scale)))
      else if cursorTransitionSpeedScaleKeys.contains(key) then
        parseElementTransitionSpeedScale(trimmed).map(scale => config.withCursorTransitionSpeedScale(Some(scale)))
      else if commandRunnerAnimationKeys.contains(key) then
        parseAnimationPreset(trimmed).map(config.withCommandRunnerAnimation)
      else if commandRunnerTransitionKeys.contains(key) then
        parseTransitionKind(trimmed).map(kind => config.withCommandRunnerTransitionKind(Some(kind)))
      else if uiAnimationKeys.contains(key) then parseAnimationPreset(trimmed).map(config.withUiAnimation)
      else if editorTextTransitionKeys.contains(key) then
        parseTransitionKind(trimmed).map(config.withEditorInsertionTransitionKind)
      else if panelOpenTransitionKeys.contains(key) then
        parseTransitionKind(trimmed).map(kind => config.withPanelOpenTransitionKind(Some(kind)))
      else if panelCloseTransitionKeys.contains(key) then
        parseTransitionKind(trimmed).map(kind => config.withPanelCloseTransitionKind(Some(kind)))
      else if commandRunnerVisibleRowsKeys.contains(key) then
        parseCommandRunnerVisibleRows(trimmed).map(config.withCommandRunnerVisibleRows)
      else if commandRunnerItemGapRowsKeys.contains(key) then
        parseCommandRunnerItemGapRows(trimmed).map(config.withCommandRunnerItemGapRows)
      else if commandRunnerCursorGapRowsKeys.contains(key) then
        parseCommandRunnerCursorGapRows(trimmed).map(config.withCommandRunnerCursorGapRows)
      else if cursorInfoBarBackgroundAlphaKeys.contains(key) then
        parseCursorInfoBarBackgroundAlpha(trimmed).map(config.withCursorInfoBarBackgroundAlpha)
      else if renderFpsKeys.contains(key) then RenderFpsTarget.fromConfigKey(trimmed).map(config.withRenderFpsTarget)
      else if renderDamageGranularityKeys.contains(key) then
        RenderDamageGranularity.fromConfigKey(trimmed).map(config.withRenderDamageGranularity)
      else if wordWrapKeys.contains(key) then parseBoolean(trimmed).map(config.withWordWrap)
      else if visualLineNavigationKeys.contains(key) then
        parseBoolean(trimmed).map(config.withVisualLineCursorNavigation)
      else if blurRadiusKeys.contains(key) then
        trimmed.toFloatOption.filter(radius => radius >= 0.0f && radius <= 1.0f).map(config.withBlurRadius)
      else if backgroundStyleKeys.contains(key) then
        BackgroundStyle.fromConfigKey(trimmed).map(config.withBackgroundStyle)
      else if lineNumberKeys.contains(key) then parseBoolean(trimmed).map(config.withLineNumbers)
      else if gutterKeys.contains(key) then parseBoolean(trimmed).map(config.withGutter)
      else if wordCountKeys.contains(key) then parseBoolean(trimmed).map(config.withWordCount)
      else if commentDisplayModeKeys.contains(key) then
        CommentDisplayMode.fromConfigKey(trimmed).map(config.withCommentDisplayMode)
      else if commandRunnerShowKeyHintsKeys.contains(key) then
        parseBoolean(trimmed).map(config.withCommandRunnerShowKeyHints)
      else if commandRunnerCursorPeekKeys.contains(key) then
        parseBoolean(trimmed).map(config.withCommandRunnerCursorPeekEnabled)
      else if commandRunnerCursorPeekModifierKeys.contains(key) then
        parseModifier(trimmed).map(config.withCommandRunnerCursorPeekModifier)
      else if commandRunnerCursorPeekTapWindowKeys.contains(key) then
        trimmed.toLongOption.map(config.withCommandRunnerCursorPeekTapWindowMillis)
      else if commandRunnerCursorPeekPlacementKeys.contains(key) then
        parseSurfacePlacement(trimmed).map(config.withCommandRunnerCursorPeekPlacement)
      else if paneHeaderKeys.contains(key) then parseBoolean(trimmed).map(config.withPaneHeaders)
      else if focusedTextBodyKeys.contains(key) then parseBoolean(trimmed).map(config.withFocusedTextBody)
      else if contextualToolbarKeys.contains(key) then parseBoolean(trimmed).map(config.withContextualToolbarEnabled)
      else if contextualToolbarModeKeys.contains(key) then
        ToolbarDisplayMode.fromConfigKey(trimmed).map(config.withContextualToolbarDisplayMode)
      else if textAreaLeftPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaLeftInset)
      else if textAreaRightPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaRightInset)
      else if textAreaTopPercentKeys.contains(key) then parseInsetPercent(trimmed).map(config.withTextAreaTopInset)
      else if textAreaBottomPercentKeys.contains(key) then
        parseInsetPercent(trimmed).map(config.withTextAreaBottomInset)
      else if viewportWidthPercentKeys.contains(key) then
        parseViewportPercent(trimmed)
          .map(percent =>
            config.withViewportWidthSizing(config.surfaceConfig.viewportSizing.width.copy(percent = percent))
          )
      else if viewportWidthMaxKeys.contains(key) then
        parseViewportMaxCells(trimmed)
          .map(maxCells =>
            config.withViewportWidthSizing(config.surfaceConfig.viewportSizing.width.copy(maxCells = maxCells))
          )
      else if viewportHeightPercentKeys.contains(key) then
        parseViewportPercent(trimmed)
          .map(percent =>
            config.withViewportHeightSizing(config.surfaceConfig.viewportSizing.height.copy(percent = percent))
          )
      else if viewportHeightMaxKeys.contains(key) then
        parseViewportMaxCells(trimmed)
          .map(maxCells =>
            config.withViewportHeightSizing(config.surfaceConfig.viewportSizing.height.copy(maxCells = maxCells))
          )
      else None

    def invalidValue(key: String, value: String): Boolean =
      parse(AppConfig.default, key, value).isEmpty

    private def parseModifier(value: String): Option[Modifier] =
      Modifier.values.find(_.toString.equalsIgnoreCase(value))

    private def parseSurfacePlacement(value: String): Option[SurfacePlacement] =
      SurfacePlacement.values.find(placement => placement.toString.equalsIgnoreCase(value.replace("-", "")))

    private def parseBoolean(value: String): Option[Boolean] =
      value.toLowerCase match
        case "true" | "on" | "enabled"    => Some(true)
        case "false" | "off" | "disabled" => Some(false)
        case _                            => None

    private def parseMotionFamily(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val parts = key.stripPrefix(motionFamilyPrefix).split("\\.")
      for
        familyName <- parts.headOption
        family     <- MotionFamily.values.find(_.configKey == familyName)
        field    = parts.drop(1).mkString(".")
        current  = config.surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(config.surfaceConfig))
        settings = current.families(family)
        updated <- field match
          case "enabled"    => parseBoolean(value).map(enabled => settings.copy(enabled = enabled))
          case "transition" => parseTransitionKind(value).map(kind => settings.copy(transitionKind = kind))
          case "animation" | "animation.preset" if value.equalsIgnoreCase("custom") =>
            Some(settings.copy(animation = Some(settings.animation.getOrElse(AnimationConfig.Enabled.smooth))))
          case "animation" | "animation.preset" =>
            parseAnimationPreset(value).map(animation => settings.copy(animation = animation))
          case "animation.duration_ms" =>
            value.toIntOption
              .filter(_ > 0)
              .map(durationMs =>
                settings.copy(animation =
                  Some(
                    settings.animation
                      .getOrElse(AnimationConfig.Enabled.smooth)
                      .copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(durationMs * 1_000_000L))
                  )
                )
              )
          case "animation.steps" =>
            value.toIntOption
              .filter(_ > 0)
              .map(steps =>
                settings
                  .copy(animation =
                    Some(settings.animation.getOrElse(AnimationConfig.Enabled.smooth).copy(steps = steps))
                  )
              )
          case "speed_scale" => parseElementTransitionSpeedScale(value).map(scale => settings.copy(speedScale = scale))
          case "open_transition" if family == MotionFamily.PinnedPanels =>
            parseTransitionKind(value).map(kind =>
              settings.copy(transitionOverrides = settings.transitionOverrides.updated(TransitionScope.PanelOpen, kind))
            )
          case "close_transition" if family == MotionFamily.PinnedPanels =>
            parseTransitionKind(value).map(kind =>
              settings.copy(transitionOverrides =
                settings.transitionOverrides.updated(TransitionScope.PanelClose, kind)
              )
            )
          case _ => None
      yield config.withMotionFamilyConfiguration(family, updated)

    private def parseCommandRunnerVisibleRows(value: String): Option[Option[Int]] =
      value.toLowerCase match
        case "auto" | "default" | "" => Some(None)
        case other =>
          other.toIntOption
            .filter(rows =>
              rows >= AppConfig.MinCommandRunnerVisibleRows &&
                rows <= AppConfig.MaxCommandRunnerVisibleRows
            )
            .map(rows => Some(rows))

    private def parseCommandRunnerItemGapRows(value: String): Option[Double] =
      value.toDoubleOption.filter(rows =>
        rows >= AppConfig.MinCommandRunnerItemGapRows && rows <= AppConfig.MaxCommandRunnerItemGapRows
      )

    private def parseCommandRunnerCursorGapRows(value: String): Option[Option[Double]] =
      value.toLowerCase match
        case "auto" | "default" | "" => Some(None)
        case other =>
          other.toDoubleOption
            .filter(rows =>
              rows >= AppConfig.MinCommandRunnerCursorGapRows && rows <= AppConfig.MaxCommandRunnerCursorGapRows
            )
            .map(rows => Some(rows))

    private def parseCursorInfoBarBackgroundAlpha(value: String): Option[Option[Double]] =
      value.toLowerCase match
        case "auto" | "default" | "" => Some(None)
        case other =>
          other.toDoubleOption
            .filter(alpha =>
              alpha >= AppConfig.MinCursorInfoBarBackgroundAlpha && alpha <= AppConfig.MaxCursorInfoBarBackgroundAlpha
            )
            .map(alpha => Some(alpha))

    /** These are stored as fractions and written as percentages, so reading one back divides by 100 -- and binary
      * floating point turns 17.3 into 0.17299999999999996 rather than the 0.173 that was saved. Rounding to the
      * precision the file actually carries makes saving and loading a settings value give that value back.
      */
    private def fractionOfPercent(value: Double): Double =
      BigDecimal(value / 100.0).setScale(9, BigDecimal.RoundingMode.HALF_UP).toDouble

    private def parseInsetPercent(value: String): Option[Double] =
      value.toDoubleOption
        .map(fractionOfPercent)
        .filter(percent => percent >= 0.0 && percent <= TextAreaInsets.MaxInset)

    private def parseViewportPercent(value: String): Option[Double] =
      value.toDoubleOption
        .map(fractionOfPercent)
        .filter(percent =>
          percent >= ViewportAxisSizing.MinPercent &&
            percent <= ViewportAxisSizing.MaxPercent
        )

    private def parseViewportMaxCells(value: String): Option[Option[Int]] =
      if value.trim.isEmpty then Some(None)
      else value.toIntOption.filter(_ >= 1).map(Some(_))

    private def parseMaterialPreset(value: String): Option[MaterialPreset] =
      value.toLowerCase match
        case "solid" | "opaque"      => Some(MaterialPreset.Solid)
        case "clear" | "transparent" => Some(MaterialPreset.Clear)
        case "frosted" | "soft"      => Some(MaterialPreset.Frosted)
        case "crystal" | "glass"     => Some(MaterialPreset.Crystal)
        case "custom"                => Some(MaterialPreset.Custom)
        case _                       => None

    private def parseMotionPreset(value: String): Option[MotionPreset] =
      value.toLowerCase match
        case "reduced" | "none" | "off" | "disabled" => Some(MotionPreset.Reduced)
        case "subtle"                                => Some(MotionPreset.Subtle)
        case "smooth"                                => Some(MotionPreset.Smooth)
        case "expressive" | "full" | "quick"         => Some(MotionPreset.Expressive)
        case "custom"                                => Some(MotionPreset.Custom)
        case _                                       => None

    private def parseAnimationPreset(value: String): Option[Option[AnimationConfig]] =
      value.toLowerCase match
        case "none" | "false" | "off" | "disabled" => Some(None)
        case "quick" | "expressive"                => Some(AnimationConfig.quick)
        case "smooth"                              => Some(AnimationConfig.smooth)
        case "subtle"                              => Some(AnimationConfig.subtle)
        case _                                     => None

    private def parseTransitionKind(value: String): Option[TransitionKind] =
      value.toLowerCase match
        case "fade"                                             => Some(TransitionKind.Fade)
        case "typed" | "typed-text" | "type"                    => Some(TransitionKind.TypedText)
        case "directional" | "directional-sweep" | "sweep"      => Some(TransitionKind.DirectionalSweep)
        case "tandem" | "line-and-character" | "line-character" => Some(TransitionKind.LineAndCharacterTandem)
        case "outline" | "outline-then-content" | "frame-then-content" =>
          Some(TransitionKind.OutlineThenContent)
        case "off" | "none" | "disabled" => Some(TransitionKind.Disabled)
        case _                           => None

    private def parseElementTransitionSpeedScale(value: String): Option[Double] =
      value.toDoubleOption
        .filter(scale =>
          scale >= AppConfig.MinElementTransitionSpeedScale &&
            scale <= AppConfig.MaxElementTransitionSpeedScale
        )

/** Global application configuration */
final case class AppConfig(
    editorConfig: EditorConfig = EditorConfig(),
    inputConfig: InputConfig = InputConfig(),
    surfaceConfig: SurfaceConfig = SurfaceConfig(),
    cursorConfig: CursorConfig = CursorConfig(),
    windowConfig: WindowConfig = WindowConfig(),
    windowSitterConfig: WindowSitterConfig = WindowSitterConfig.default,
    documentConfig: DocumentConfig = DocumentConfig(),
    interfaceConfig: InterfaceConfig = InterfaceConfig(),
    languageToolsConfig: LanguageToolsConfig = LanguageToolsConfig(),
    appModeConfig: AppModeConfig = AppModeConfig()
):

  def withEditorConfig(config: EditorConfig): AppConfig =
    copy(editorConfig = config.normalized)

  def withLanguageToolsConfig(config: LanguageToolsConfig): AppConfig =
    copy(languageToolsConfig = config.normalized)

  def withInputConfig(config: InputConfig): AppConfig =
    copy(inputConfig = config)

  def withSurfaceConfig(config: SurfaceConfig): AppConfig =
    copy(surfaceConfig = config.normalized)

  def windowChromeMode: WindowChromeMode =
    windowConfig.chromeMode

  def preferredWindowSize: Option[PreferredWindowSize] =
    windowConfig.preferredSize

  def markdownViewMode: MarkdownViewMode =
    documentConfig.markdownViewMode

  def defaultDocumentMode: DefaultDocumentMode =
    documentConfig.defaultMode

  def appMode: AppMode =
    appModeConfig.mode

  def showAllSettingsRegardlessOfMode: Boolean =
    appModeConfig.showAllSettingsRegardlessOfMode

  def interfaceDensity: InterfaceDensity =
    interfaceConfig.density

  def uiElementGap: Double =
    interfaceConfig.elementGap

  def uiCornerRadiusPx: Int =
    interfaceConfig.cornerRadiusPx

  def uiOutlineThicknessPx: Int =
    interfaceConfig.outlineThicknessPx

  /** Create a new config with character animation enabled */
  def withCharacterAnimation(config: AnimationConfig): AppConfig =
    val updated = withEditorConfig(editorConfig.copy(characterAnimation = Some(config)))
      .withSurfaceConfig(surfaceConfig.copy(motionPreset = MotionPreset.Custom))
    updated.updateAuthoritativeMotion(identity) { configuration =>
      updated.updateMotionFamily(configuration, MotionFamily.EditorText)(_.copy(animation = Some(config)))
    }

  /** Create a new config with character animation disabled */
  def withoutCharacterAnimation: AppConfig =
    val updated = withEditorConfig(editorConfig.copy(characterAnimation = None)).withSurfaceConfig(
      surfaceConfig.copy(
        motionPreset = MotionPreset.Reduced,
        editorTextTransitionSpeedScale = None,
        commandRunnerTransitionSpeedScale = None,
        uiTransitionSpeedScale = None,
        cursorTransitionSpeedScale = None,
        commandRunnerAnimation = None,
        uiAnimation = None,
        commandRunnerTransitionKind = None
      )
    )
    updated.updateAuthoritativeMotion(identity) { configuration =>
      updated.updateMotionFamily(
        configuration.copy(baseline = MotionPreset.Reduced),
        MotionFamily.EditorText
      )(_.disabled)
    }

  /** Create a new config with syntax highlighting toggled */
  def withSyntaxHighlighting(enabled: Boolean): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(syntaxHighlightingEnabled = enabled))

  def withHotkeyConfig(config: HotkeyConfig): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = config))

  def withHotkeyOverride(action: HotkeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = inputConfig.hotkeyConfig.withBinding(action, binding)))

  def withHotkeyOverrideUnbindingConflicts(action: HotkeyAction, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(hotkeyConfig = inputConfig.hotkeyConfig.withBindingUnbindingConflicts(action, binding))
    )

  def resetHotkeyOverride(action: HotkeyAction): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = inputConfig.hotkeyConfig.resetBinding(action)))

  def withFocusedKeymapConfig(config: FocusedKeymapConfig): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = config))

  def withKeymapBinding[A <: KeymapEventAction[E], E <: Event](
    group: KeymapGroup[A, E]
  )(action: A, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig = inputConfig.focusedKeymapConfig.withBinding(group)(action, binding))
    )

  def withKeymapBindingUnbindingConflicts[A <: KeymapEventAction[E], E <: Event](
    group: KeymapGroup[A, E]
  )(action: A, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig =
        inputConfig.focusedKeymapConfig.withBindingUnbindingConflicts(group)(action, binding)
      )
    )

  def resetKeymapBinding[A <: KeymapEventAction[E], E <: Event](group: KeymapGroup[A, E])(action: A): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig = inputConfig.focusedKeymapConfig.resetBinding(group)(action))
    )

  /** Create a new config with font configuration */
  def withFontConfig(config: FontConfig): AppConfig =
    withEditorConfig(editorConfig.copy(fontConfig = config))

  /** Create a new config with minimum pane width setting */
  def withMinimumPaneWidth(width: Int): AppConfig =
    withEditorConfig(editorConfig.copy(minimumPaneWidth = width))

  /** Create a new config with line numbers toggled */
  def withLineNumbers(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(showLineNumbers = enabled))

  /** Create a new config with gutter toggled */
  def withGutter(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(showGutter = enabled))

  /** Show or hide the per-pane identity strip above editor content. */
  def withPaneHeaders(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(showPaneHeaders = enabled))

  /** Show or hide the word/character-count and reading-time segment in the status bar (#1203). */
  def withWordCount(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(showWordCount = enabled))

  /** Selects how document comments become visible: floating on-demand lens or persistent margin (#1222). */
  def withCommentDisplayMode(mode: CommentDisplayMode): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commentDisplayMode = mode))

  /** Create a new config with word wrapping toggled */
  def withWordWrap(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(wordWrapEnabled = enabled))

  def withVisualLineCursorNavigation(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(visualLineCursorNavigation = enabled))

  def withTypewriterScrolling(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(typewriterScrollingEnabled = enabled))

  /** `None` restores the active theme's own panel alpha for the cursor info bar; `Some` overrides just that one panel's
    * background alpha, independent of theme.
    */
  def withCursorInfoBarBackgroundAlpha(alpha: Option[Double]): AppConfig =
    withSurfaceConfig(
      surfaceConfig.copy(cursorInfoBarBackgroundAlpha = alpha.map(AppConfig.clampCursorInfoBarBackgroundAlpha))
    )

  def withFocusedTextBody(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(focusedTextBodyEnabled = enabled))

  /** Show or hide the command runner's persistent key-hint footer row (issue #931, Stage 3). */
  def withCommandRunnerShowKeyHints(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerShowKeyHints = enabled))

  /** Enable or disable the experimental cursor-peek prototype (off by default). */
  def withCommandRunnerCursorPeekEnabled(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorPeekEnabled = enabled))

  def withCommandRunnerCursorPeekModifier(modifier: Modifier): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorPeekModifier = modifier))

  def withCommandRunnerCursorPeekTapWindowMillis(millis: Long): AppConfig =
    withSurfaceConfig(
      surfaceConfig.copy(commandRunnerCursorPeekTapWindowMillis =
        AppConfig.clampCommandRunnerCursorPeekTapWindowMillis(millis)
      )
    )

  def withCommandRunnerCursorPeekPlacement(placement: SurfacePlacement): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorPeekPlacement = placement))

  def withContextualToolbarEnabled(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(contextualToolbarEnabled = enabled))

  def withContextualToolbarDisplayMode(mode: ToolbarDisplayMode): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(contextualToolbarDisplayMode = mode))

  def withBlurRadius(r: Float): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(blurRadius = r, materialPreset = MaterialPreset.Custom))

  def withBackgroundStyle(style: BackgroundStyle): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(backgroundStyle = style, materialPreset = MaterialPreset.Custom))

  def withMaterialPreset(preset: MaterialPreset): AppConfig =
    preset match
      case MaterialPreset.Custom =>
        withSurfaceConfig(surfaceConfig.copy(materialPreset = MaterialPreset.Custom))
      case _ =>
        withSurfaceConfig(
          surfaceConfig.copy(
            materialPreset = preset,
            backgroundStyle = preset.backgroundStyle,
            blurRadius = preset.blurRadius
          )
        )

  def withPostProcessingEffect(effect: PostProcessingEffect): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(postProcessingEffect = effect))

  def withUiShadowsEnabled(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(uiShadowsEnabled = enabled))

  def withMotionPreset(preset: MotionPreset): AppConfig =
    preset match
      case MotionPreset.Custom =>
        withSurfaceConfig(surfaceConfig.copy(motionPreset = MotionPreset.Custom))
      case _ =>
        val accessibility = surfaceConfig.motionConfiguration
          .map(_.accessibility)
          .getOrElse(MotionAccessibility.Standard)
        // A preset sets the baseline; a per-family speed the user set explicitly is not part of that baseline and
        // survives it. The legacy `Option` fields are exactly the record of which ones were set explicitly, so they
        // are re-applied over the preset's own hierarchy -- otherwise choosing a preset silently discarded them from
        // the hierarchy the renderer and the saved file read, while the legacy fields kept claiming they were in
        // force.
        val presetConfiguration =
          withExplicitFamilySpeeds(MotionConfig.forPreset(preset).copy(accessibility = accessibility))
        withEditorConfig(editorConfig.copy(characterAnimation = preset.animationConfig)).withSurfaceConfig(
          surfaceConfig.copy(
            motionPreset = preset,
            motionConfiguration = Some(presetConfiguration),
            commandRunnerAnimation = preset.animationConfig,
            uiAnimation = preset.animationConfig
          )
        )

  private def withExplicitFamilySpeeds(configuration: MotionConfig): MotionConfig =
    val overrides = List(
      MotionFamily.EditorText      -> surfaceConfig.editorTextTransitionSpeedScale,
      MotionFamily.CommandSurfaces -> surfaceConfig.commandRunnerTransitionSpeedScale,
      MotionFamily.UiTransitions   -> surfaceConfig.uiTransitionSpeedScale,
      MotionFamily.Cursor          -> surfaceConfig.cursorTransitionSpeedScale
    )
    overrides.foldLeft(configuration) {
      case (current, (family, Some(scale))) =>
        current.copy(families = current.families.updated(family, current.families(family).copy(speedScale = scale)))
      case (current, _) => current
    }

  /** Marks the current resolved family values as a custom motion baseline. */
  def withCustomMotionBaseline: AppConfig =
    val fallback = MotionConfig.fromLegacy(surfaceConfig)
    val current = surfaceConfig.motionConfiguration
      .getOrElse(fallback)
      .withFallback(fallback)
    val editorText = surfaceConfig.motionConfiguration
      .flatMap(_.families.get(MotionFamily.EditorText))
      .getOrElse(current.families(MotionFamily.EditorText).copy(animation = editorConfig.characterAnimation))
    withSurfaceConfig(
      surfaceConfig.copy(
        motionPreset = MotionPreset.Custom,
        motionConfiguration = Some(
          current
            .copy(
              baseline = MotionPreset.Custom,
              families = current.families.updated(MotionFamily.EditorText, editorText)
            )
            .normalized
        )
      )
    )

  /** Transition policy derived from the selected motion preset and UI speed scale. */
  def elementTransitionSettings: ElementTransitionSettings =
    surfaceConfig.elementTransitionSettings

  /** Transition policy derived from the selected motion preset and editor text speed scale. */
  def editorInsertionTransitionSettings: ElementTransitionSettings =
    surfaceConfig.editorInsertionTransitionSettings

  /** Transition policy for pinned panel creation. */
  def pinnedPanelTransitionSettings: ElementTransitionSettings =
    surfaceConfig.pinnedPanelTransitionSettings

  def withElementTransitionSpeedScale(scale: Double): AppConfig =
    updateAuthoritativeMotion(_.copy(elementTransitionSpeedScale = scale)) { configuration =>
      configuration.copy(families = configuration.families.view.mapValues(_.copy(speedScale = scale)).toMap)
    }

  def withEditorTextTransitionSpeedScale(scale: Option[Double]): AppConfig =
    updateAuthoritativeMotion(_.copy(editorTextTransitionSpeedScale = scale)) { configuration =>
      updateMotionFamily(configuration, MotionFamily.EditorText)(
        _.copy(speedScale = scale.getOrElse(surfaceConfig.elementTransitionSpeedScale))
      )
    }

  def withCommandRunnerTransitionSpeedScale(scale: Option[Double]): AppConfig =
    updateAuthoritativeMotion(_.copy(commandRunnerTransitionSpeedScale = scale)) { configuration =>
      updateMotionFamily(configuration, MotionFamily.CommandSurfaces)(
        _.copy(speedScale = scale.getOrElse(surfaceConfig.elementTransitionSpeedScale))
      )
    }

  def withUiTransitionSpeedScale(scale: Option[Double]): AppConfig =
    updateAuthoritativeMotion(_.copy(uiTransitionSpeedScale = scale)) { configuration =>
      val speed = scale.getOrElse(surfaceConfig.elementTransitionSpeedScale)
      updateMotionFamily(configuration, MotionFamily.UiTransitions)(_.copy(speedScale = speed))
    }

  def withCursorTransitionSpeedScale(scale: Option[Double]): AppConfig =
    updateAuthoritativeMotion(_.copy(cursorTransitionSpeedScale = scale)) { configuration =>
      updateMotionFamily(configuration, MotionFamily.Cursor)(
        _.copy(speedScale = scale.getOrElse(surfaceConfig.elementTransitionSpeedScale))
      )
    }

  def withMotionConfiguration(configuration: MotionConfig): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(motionConfiguration = Some(configuration.normalized)))

  def withMotionAccessibility(accessibility: MotionAccessibility): AppConfig =
    val current = surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(surfaceConfig))
    withMotionConfiguration(current.copy(accessibility = accessibility))

  def withMotionFamilyConfiguration(family: MotionFamily, configuration: MotionFamilyConfig): AppConfig =
    val current = surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(surfaceConfig))
    withMotionConfiguration(current.copy(families = current.families.updated(family, configuration)))

  /** Updates editor text timing in both the legacy field and the authoritative motion family. */
  def withEditorTextAnimation(animation: Option[AnimationConfig]): AppConfig =
    val updated  = withEditorConfig(editorConfig.copy(characterAnimation = animation))
    val fallback = MotionConfig.fromLegacy(updated.surfaceConfig)
    val configuration = updated.surfaceConfig.motionConfiguration
      .getOrElse(fallback)
      .withFallback(fallback)
    updated.withMotionConfiguration(
      updated.updateMotionFamily(configuration, MotionFamily.EditorText)(_.copy(animation = animation))
    )

  def withCommandRunnerAnimation(animation: Option[AnimationConfig]): AppConfig =
    updateAuthoritativeMotion(_.copy(commandRunnerAnimation = animation)) { configuration =>
      updateMotionFamily(configuration, MotionFamily.CommandSurfaces)(_.copy(animation = animation))
    }

  def withUiAnimation(animation: Option[AnimationConfig]): AppConfig =
    updateAuthoritativeMotion(_.copy(uiAnimation = animation)) { configuration =>
      updateMotionFamily(configuration, MotionFamily.UiTransitions)(_.copy(animation = animation))
    }

  def withCommandRunnerVisibleRows(rows: Option[Int]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerVisibleRows = rows))

  def withCommandRunnerItemGapRows(rows: Double): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerItemGapRows = rows))

  def withCommandRunnerCursorGapRows(rows: Option[Double]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorGapRows = rows))

  def withRenderFpsTarget(target: RenderFpsTarget): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(renderFpsTarget = target))

  def withRenderDamageGranularity(granularity: RenderDamageGranularity): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(renderDamageGranularity = granularity))

  def effectiveEditorTextTransitionSpeedScale: Double =
    surfaceConfig.effectiveEditorTextTransitionSpeedScale

  def effectiveCommandRunnerTransitionSpeedScale: Double =
    surfaceConfig.effectiveCommandRunnerTransitionSpeedScale

  def effectiveUiTransitionSpeedScale: Double =
    surfaceConfig.effectiveUiTransitionSpeedScale

  def effectiveCursorTransitionSpeedScale: Double =
    surfaceConfig.effectiveCursorTransitionSpeedScale

  /** Character insertion animation after applying the effective editor text motion speed. */
  def scaledCharacterAnimation: Option[AnimationConfig] =
    surfaceConfig.motionConfiguration match
      case Some(_) =>
        val motion = surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.EditorText)
        Option.when(motion.enabled)(AppConfig.scaledAnimation(motion.animation, motion.speedScale)).flatten
      case None => AppConfig.scaledAnimation(editorConfig.characterAnimation, effectiveEditorTextTransitionSpeedScale)

  /** Command runner animation after applying the effective command runner motion speed. */
  def scaledCommandRunnerAnimation: Option[AnimationConfig] =
    val motion = surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.CommandSurfaces)
    Option.when(motion.enabled)(AppConfig.scaledAnimation(motion.animation, motion.speedScale)).flatten

  /** General UI animation after applying the effective UI motion speed. */
  def scaledUiAnimation: Option[AnimationConfig] =
    val motion = surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions)
    Option.when(motion.enabled)(AppConfig.scaledAnimation(motion.animation, motion.speedScale)).flatten

  def withEditorInsertionTransitionKind(kind: TransitionKind): AppConfig =
    updateAuthoritativeMotion(_.copy(editorInsertionTransitionKind = kind)) { configuration =>
      updateMotionFamily(configuration, MotionFamily.EditorText)(
        _.copy(enabled = kind != TransitionKind.Disabled, transitionKind = kind)
      )
    }

  def withCommandRunnerTransitionKind(kind: Option[TransitionKind]): AppConfig =
    updateAuthoritativeMotion(_.copy(commandRunnerTransitionKind = kind)) { configuration =>
      kind.fold(configuration)(transition =>
        updateMotionFamily(configuration, MotionFamily.CommandSurfaces)(
          _.copy(
            enabled = transition != TransitionKind.Disabled,
            transitionKind = transition
          )
        )
      )
    }

  def effectiveCommandRunnerTransitionKind: TransitionKind =
    surfaceConfig.effectiveCommandRunnerTransitionKind

  def withPanelOpenTransitionKind(kind: Option[TransitionKind]): AppConfig =
    updateAuthoritativeMotion(_.copy(panelOpenTransitionKind = kind)) { configuration =>
      kind.fold(configuration)(transition =>
        updatePanelTransition(configuration, TransitionScope.PanelOpen, transition)
      )
    }

  def withPanelCloseTransitionKind(kind: Option[TransitionKind]): AppConfig =
    updateAuthoritativeMotion(_.copy(panelCloseTransitionKind = kind)) { configuration =>
      kind.fold(configuration)(transition =>
        updatePanelTransition(configuration, TransitionScope.PanelClose, transition)
      )
    }

  def effectivePanelOpenTransitionKind: TransitionKind =
    surfaceConfig.effectivePanelOpenTransitionKind

  def effectivePanelCloseTransitionKind: TransitionKind =
    surfaceConfig.effectivePanelCloseTransitionKind

  /** One line is the least a notch can usefully move; the upper bound keeps a mis-typed value from turning a notch into
    * a jump across the document.
    */
  def withWheelScrollLines(lines: Int): AppConfig =
    withInputConfig(inputConfig.copy(wheelScrollLines = AppConfig.clampWheelScrollLines(lines)))

  /** Apply a motion change to both the legacy field that describes it and the authoritative hierarchy.
    *
    * The hierarchy is materialised from the legacy fields when there is none yet. Updating it only when one already
    * existed meant a change made before any hierarchy was built -- setting an element-wide speed on a fresh
    * configuration, say -- landed in the legacy field alone, and the next thing to install a hierarchy (choosing a
    * motion preset) silently dropped it: the configuration then held one value in its legacy field and another in the
    * hierarchy the renderer and the saved file both use.
    */
  private def updateAuthoritativeMotion(
    updateSurface: SurfaceConfig => SurfaceConfig
  )(
    updateConfiguration: MotionConfig => MotionConfig
  ): AppConfig =
    val updatedSurface       = updateSurface(surfaceConfig)
    val current              = surfaceConfig.motionConfiguration.getOrElse(MotionConfig.fromLegacy(surfaceConfig))
    val fallback             = MotionConfig.fromLegacy(surfaceConfig, current.baseline)
    val updatedConfiguration = updateConfiguration(current.withFallback(fallback)).normalized
    withSurfaceConfig(updatedSurface.copy(motionConfiguration = Some(updatedConfiguration)))

  private def updateMotionFamily(
    configuration: MotionConfig,
    family: MotionFamily
  )(
    update: MotionFamilyConfig => MotionFamilyConfig
  ): MotionConfig =
    configuration.copy(families = configuration.families.updated(family, update(configuration.families(family))))

  private def updatePanelTransition(
    configuration: MotionConfig,
    scope: TransitionScope,
    transition: TransitionKind
  ): MotionConfig =
    updateMotionFamily(configuration, MotionFamily.PinnedPanels) { panel =>
      val overrides = panel.transitionOverrides.updated(scope, transition)
      panel.copy(
        enabled = overrides.values.exists(_ != TransitionKind.Disabled),
        transitionKind = overrides.getOrElse(TransitionScope.PanelOpen, panel.transitionKind),
        transitionOverrides = overrides
      )
    }

  def cursorMode: CursorMode =
    cursorConfig.mode

  def cursorColors: CursorColorConfig =
    cursorConfig.colors

  def cursorInfoBarSegments: List[CursorInfoBarSegment] =
    cursorConfig.infoBarSegments

  def cursorInfoBarPlacement: CursorInfoBarPlacement =
    cursorConfig.infoBarPlacement

  def cursorInfoBarColors: CursorInfoBarColorConfig =
    cursorConfig.infoBarColors

  def withCursorConfig(config: CursorConfig): AppConfig =
    copy(cursorConfig = config)

  def withCursorMode(mode: CursorMode): AppConfig =
    withCursorConfig(cursorConfig.copy(mode = mode))

  def withCursorColors(colors: CursorColorConfig): AppConfig =
    withCursorConfig(cursorConfig.copy(colors = colors))

  def withCursorInfoBarSegments(segments: List[CursorInfoBarSegment]): AppConfig =
    withCursorConfig(cursorConfig.copy(infoBarSegments = segments))

  def withCursorInfoBarPlacement(placement: CursorInfoBarPlacement): AppConfig =
    withCursorConfig(cursorConfig.copy(infoBarPlacement = placement))

  def withCursorInfoBarColors(colors: CursorInfoBarColorConfig): AppConfig =
    withCursorConfig(cursorConfig.copy(infoBarColors = colors))

  def withWindowConfig(config: WindowConfig): AppConfig =
    copy(windowConfig = config.normalized)

  def withWindowChromeMode(mode: WindowChromeMode): AppConfig =
    withWindowConfig(windowConfig.copy(chromeMode = mode))

  def withDocumentConfig(config: DocumentConfig): AppConfig =
    copy(documentConfig = config)

  def withMarkdownViewMode(mode: MarkdownViewMode): AppConfig =
    withDocumentConfig(documentConfig.copy(markdownViewMode = mode))

  /** Create a new config with the default mode used for new empty buffers. */
  def withDefaultDocumentMode(mode: DefaultDocumentMode): AppConfig =
    withDocumentConfig(documentConfig.copy(defaultMode = mode))

  def withAppModeConfig(config: AppModeConfig): AppConfig =
    copy(appModeConfig = config)

  def withAppMode(mode: AppMode): AppConfig =
    withAppModeConfig(appModeConfig.copy(mode = mode))

  def withShowAllSettingsRegardlessOfMode(value: Boolean): AppConfig =
    withAppModeConfig(appModeConfig.copy(showAllSettingsRegardlessOfMode = value))

  def withInterfaceConfig(config: InterfaceConfig): AppConfig =
    copy(interfaceConfig = config.normalized)

  def withInterfaceDensity(density: InterfaceDensity): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(density = density))

  def withUiElementGap(gap: Double): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(elementGap = gap))

  def withUiCornerRadiusPx(radius: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(cornerRadiusPx = radius))

  def withUiOutlineThicknessPx(thickness: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(outlineThicknessPx = thickness))

  def withTextAreaInsets(insets: TextAreaInsets): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(textAreaInsets = insets))

  def withTextAreaLeftInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(left = value))

  def withTextAreaRightInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(right = value))

  def withTextAreaTopInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(top = value))

  def withTextAreaBottomInset(value: Double): AppConfig =
    withTextAreaInsets(surfaceConfig.textAreaInsets.copy(bottom = value))

  def withViewportSizing(sizing: ViewportSizing): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(viewportSizing = sizing))

  def withViewportWidthSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(surfaceConfig.viewportSizing.copy(width = sizing))

  def withViewportHeightSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(surfaceConfig.viewportSizing.copy(height = sizing))

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    withWindowConfig(windowConfig.copy(preferredSize = Some(size.normalized)))

  def withWindowSitterConfig(config: WindowSitterConfig): AppConfig =
    copy(windowSitterConfig = config.normalized)

  def withLspUserConfig(config: LspUserConfig): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(lspUserConfig = config))

  def withSpellCheck(config: SpellCheckConfig): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(spellCheck = config))

object AppConfig:

  val MinElementTransitionSpeedScale: Double  = 0.0
  val MaxElementTransitionSpeedScale: Double  = 4.0
  val MinUiElementGap: Double                 = 0.0
  val MaxUiElementGap: Double                 = 8.0
  val MinUiCornerRadiusPx: Int                = 0
  val MaxUiCornerRadiusPx: Int                = 32
  val MinUiOutlineThicknessPx: Int            = 1
  val MaxUiOutlineThicknessPx: Int            = 8
  val MinCommandRunnerVisibleRows: Int        = 1
  val MaxCommandRunnerVisibleRows: Int        = 20
  val MinCommandRunnerItemGapRows: Double     = 0.0
  val MaxCommandRunnerItemGapRows: Double     = 8.0
  val MinCommandRunnerCursorGapRows: Double   = 0.0
  val MaxCommandRunnerCursorGapRows: Double   = 8.0
  val MinCursorInfoBarBackgroundAlpha: Double = 0.0
  val MaxCursorInfoBarBackgroundAlpha: Double = 1.0
  // Wide enough to allow a deliberately slow "hold" feel while still rejecting nonsensical (near-zero or
  // multi-second) values; 200 (the default, matching `ModifierTapDetector.WindowMillis`) sits well inside it.
  val MinCommandRunnerCursorPeekTapWindowMillis: Long = 50L
  val MaxCommandRunnerCursorPeekTapWindowMillis: Long = 2000L

  def clampElementTransitionSpeedScale(scale: Double): Double =
    scale.max(MinElementTransitionSpeedScale).min(MaxElementTransitionSpeedScale)

  def clampUiElementGap(gap: Double): Double =
    if gap.isFinite then gap.max(MinUiElementGap).min(MaxUiElementGap) else MinUiElementGap

  def clampUiCornerRadiusPx(radius: Int): Int =
    radius.max(MinUiCornerRadiusPx).min(MaxUiCornerRadiusPx)

  def clampUiOutlineThicknessPx(thickness: Int): Int =
    thickness.max(MinUiOutlineThicknessPx).min(MaxUiOutlineThicknessPx)

  def clampWheelScrollLines(lines: Int): Int =
    lines.max(1).min(50)

  def clampCommandRunnerVisibleRows(rows: Int): Int =
    rows.max(MinCommandRunnerVisibleRows).min(MaxCommandRunnerVisibleRows)

  def clampCursorInfoBarBackgroundAlpha(alpha: Double): Double =
    if alpha.isFinite then alpha.max(MinCursorInfoBarBackgroundAlpha).min(MaxCursorInfoBarBackgroundAlpha)
    else MinCursorInfoBarBackgroundAlpha

  def clampCommandRunnerItemGapRows(rows: Double): Double =
    if rows.isFinite then rows.max(MinCommandRunnerItemGapRows).min(MaxCommandRunnerItemGapRows)
    else MinCommandRunnerItemGapRows

  def clampCommandRunnerCursorGapRows(rows: Double): Double =
    if rows.isFinite then rows.max(MinCommandRunnerCursorGapRows).min(MaxCommandRunnerCursorGapRows)
    else MinCommandRunnerCursorGapRows

  def clampCommandRunnerCursorPeekTapWindowMillis(millis: Long): Long =
    millis.max(MinCommandRunnerCursorPeekTapWindowMillis).min(MaxCommandRunnerCursorPeekTapWindowMillis)

  def scaledAnimation(animation: Option[AnimationConfig], speedScale: Double): Option[AnimationConfig] =
    animation.flatMap(_.scaledBy(clampElementTransitionSpeedScale(speedScale)))

  /** Default configuration keeps text entry immediate and uses restrained frosted surfaces. */
  /** What the app ships with.
    *
    * Only the settings that differ from their own field's default belong here. Restating one that already matches hides
    * which of the two is the real answer -- four of these used to, and telling them apart meant reading both.
    * `ConfigRegistry.defaults` lists every setting's default, and `docs/default-config.conf` is generated from it.
    */
  val default: AppConfig = AppConfig(
    surfaceConfig = SurfaceConfig(motionPreset = MotionPreset.Smooth)
  )

  /** Test configuration with visible animations enabled */
  val withTestAnimations: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.quick),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.quick,
      motionPreset = MotionPreset.Expressive
    )
  )

  /** Quick fade-in animation configuration */
  val withQuickAnimation: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.quick),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.quick,
      motionPreset = MotionPreset.Expressive
    )
  )

  /** Smooth fade-in animation configuration */
  val withSmoothAnimation: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.smooth),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.smooth,
      motionPreset = MotionPreset.Smooth
    )
  )

  /** Subtle fade-in animation configuration */
  val withSubtleAnimation: AppConfig = AppConfig(
    editorConfig = EditorConfig(characterAnimation = AnimationConfig.subtle),
    surfaceConfig = SurfaceConfig(
      uiAnimation = AnimationConfig.subtle,
      motionPreset = MotionPreset.Subtle
    )
  )
