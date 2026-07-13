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

enum PostProcessingEffect(val configKey: String):
  case Off       extends PostProcessingEffect("off")
  case Scanlines extends PostProcessingEffect("scanlines")
  case Glow      extends PostProcessingEffect("glow")

object PostProcessingEffect:

  def fromConfigKey(value: String): Option[PostProcessingEffect] =
    value.trim.toLowerCase match
      case "off" | "none" | "disabled"      => Some(PostProcessingEffect.Off)
      case "scanlines" | "scanline" | "crt" => Some(PostProcessingEffect.Scanlines)
      case "glow"                           => Some(PostProcessingEffect.Glow)
      case _                                => None

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

enum CursorMode(val configKey: String):
  case Blink   extends CursorMode("blink")
  case Breathe extends CursorMode("breathe")

object CursorMode:

  def fromConfigKey(value: String): Option[CursorMode] =
    value.trim.toLowerCase match
      case "blink"                 => Some(CursorMode.Blink)
      case "breathe" | "breathing" => Some(CursorMode.Breathe)
      case _                       => None

enum CursorInfoBarMode:
  case Off
  case Position
  case Detailed

  def configKey: String =
    this match
      case Off      => "off"
      case Position => "position"
      case Detailed => "detailed"

object CursorInfoBarMode:

  def fromConfigKey(value: String): Option[CursorInfoBarMode] =
    value.trim.toLowerCase match
      case "off" | "false" | "disabled" =>
        Some(CursorInfoBarMode.Off)
      case "position" | "minimal" =>
        Some(CursorInfoBarMode.Position)
      case "detailed" | "full" =>
        Some(CursorInfoBarMode.Detailed)
      case _ =>
        None

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

enum WindowChromeMode(val configKey: String):
  case Native       extends WindowChromeMode("native")
  case NativeThemed extends WindowChromeMode("native-themed")
  case Custom       extends WindowChromeMode("custom")

object WindowChromeMode:

  def fromConfigKey(value: String): Option[WindowChromeMode] =
    value.trim.toLowerCase match
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

case class LanguageToolsConfig(
    syntaxHighlightingEnabled: Boolean = false,
    lspUserConfig: LspUserConfig = LspUserConfig.empty,
    spellCheck: SpellCheckConfig = SpellCheckConfig()
):

  def normalized: LanguageToolsConfig =
    copy(spellCheck = spellCheck.normalized)

object LanguageToolsConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "syntax.highlighting",
      "spellcheck.enabled",
      "spellcheck.languages",
      "spellcheck.dictionary_paths",
      "spellcheck.dictionary.paths",
      "spellcheck.words"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "syntax_highlighting"         -> "syntax.highlighting",
      "spellcheck_enabled"          -> "spellcheck.enabled",
      "spellcheck_languages"        -> "spellcheck.languages",
      "spellcheck_dictionary_paths" -> "spellcheck.dictionary_paths",
      "spellcheck_words"            -> "spellcheck.words"
    )

    val syntaxHighlightingKeys: Set[String] = Set("syntax.highlighting", "syntax_highlighting")

    val spellCheckEnabledKeys: Set[String] = Set("spellcheck.enabled", "spellcheck_enabled")

    val spellCheckLanguageKeys: Set[String] = Set("spellcheck.languages", "spellcheck_languages")

    val spellCheckDictionaryPathKeys: Set[String] =
      Set("spellcheck.dictionary_paths", "spellcheck.dictionary.paths", "spellcheck_dictionary_paths")

    val spellCheckWordKeys: Set[String] = Set("spellcheck.words", "spellcheck_words")

    val dynamicPrefixes: List[String] = List("lsp.")

    private val handledKeys: Set[String] =
      syntaxHighlightingKeys ++
        spellCheckEnabledKeys ++
        spellCheckLanguageKeys ++
        spellCheckDictionaryPathKeys ++
        spellCheckWordKeys

    def handles(key: String): Boolean =
      handledKeys.contains(key)

    def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val trimmed = value.trim
      if syntaxHighlightingKeys.contains(key) then parseBoolean(trimmed).map(config.withSyntaxHighlighting)
      else if spellCheckEnabledKeys.contains(key) then
        parseBoolean(trimmed)
          .map(enabled => config.withSpellCheck(config.spellCheck.copy(enabled = enabled)))
      else if spellCheckLanguageKeys.contains(key) then
        Some(config.withSpellCheck(config.spellCheck.copy(languages = parseCommaList(trimmed))))
      else if spellCheckDictionaryPathKeys.contains(key) then
        Some(config.withSpellCheck(config.spellCheck.copy(dictionaryPaths = parseCommaListPreserveCase(trimmed))))
      else if spellCheckWordKeys.contains(key) then
        Some(config.withSpellCheck(config.spellCheck.copy(additionalWords = parseCommaList(trimmed))))
      else None

    def invalidValue(key: String, value: String): Boolean =
      parse(AppConfig.default, key, value).isEmpty

    private def parseBoolean(value: String): Option[Boolean] =
      value.toLowerCase match
        case "true" | "on" | "enabled"    => Some(true)
        case "false" | "off" | "disabled" => Some(false)
        case _                            => None

    private def parseCommaList(value: String): List[String] =
      value.split(',').toList.map(_.trim.toLowerCase).filter(_.nonEmpty)

    private def parseCommaListPreserveCase(value: String): List[String] =
      value.split(',').toList.map(_.trim).filter(_.nonEmpty)

case class PreferredWindowSize(width: Int, height: Int):
  def normalized: PreferredWindowSize =
    PreferredWindowSize(width.max(400), height.max(300))

case class WindowConfig(
    chromeMode: WindowChromeMode = WindowChromeMode.Native,
    preferredSize: Option[PreferredWindowSize] = None
):

  def normalized: WindowConfig =
    copy(preferredSize = preferredSize.map(_.normalized))

object WindowConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "window.chrome",
      "window.chrome.mode",
      "window.preferred.width",
      "window.preferred.height"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "window_chrome"           -> "window.chrome",
      "window_chrome_mode"      -> "window.chrome",
      "window_preferred_width"  -> "window.preferred.width",
      "window_preferred_height" -> "window.preferred.height"
    )

    val chromeKeys: Set[String] = Set("window.chrome", "window.chrome.mode") ++ deprecatedKeys.keySet.filter(
      _.startsWith("window_chrome")
    )

    val preferredWidthKeys: Set[String] = Set("window.preferred.width", "window_preferred_width")

    val preferredHeightKeys: Set[String] = Set("window.preferred.height", "window_preferred_height")

    private val handledKeys: Set[String] = chromeKeys ++ preferredWidthKeys ++ preferredHeightKeys

    def handles(key: String): Boolean =
      handledKeys.contains(key)

    def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val trimmed = value.trim
      if chromeKeys.contains(key) then WindowChromeMode.fromConfigKey(trimmed).map(config.withWindowChromeMode)
      else if preferredWidthKeys.contains(key) then
        trimmed.toIntOption.map { width =>
          config.withPreferredWindowSize(
            config.preferredWindowSize.getOrElse(PreferredWindowSize(width, 768)).copy(width = width)
          )
        }
      else if preferredHeightKeys.contains(key) then
        trimmed.toIntOption.map { height =>
          config.withPreferredWindowSize(
            config.preferredWindowSize.getOrElse(PreferredWindowSize(1024, height)).copy(height = height)
          )
        }
      else None

    def invalidValue(key: String, value: String): Boolean =
      val trimmed = value.trim
      if chromeKeys.contains(key) then WindowChromeMode.fromConfigKey(trimmed).isEmpty
      else if preferredWidthKeys.contains(key) || preferredHeightKeys.contains(key) then
        trimmed.nonEmpty && trimmed.toIntOption.isEmpty
      else false

case class CursorColorConfig(
    active: Option[Color] = None,
    inactive: Option[Color] = None
):
  def activeOr(default: Color): Color =
    active.getOrElse(default)

  def inactiveOr(activeColor: Color): Color =
    inactive.getOrElse(activeColor)

case class CursorConfig(
    mode: CursorMode = CursorMode.Blink,
    colors: CursorColorConfig = CursorColorConfig(),
    infoBarMode: CursorInfoBarMode = CursorInfoBarMode.Off,
    infoBarPlacement: CursorInfoBarPlacement = CursorInfoBarPlacement.Floating
)

object CursorConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "cursor.mode",
      "cursor.active.color",
      "cursor.inactive.color",
      "cursor.info_bar",
      "cursor.info.bar",
      "cursor.info_bar.placement",
      "cursor.info.bar.placement"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "cursor_mode"               -> "cursor.mode",
      "cursor_active_color"       -> "cursor.active.color",
      "cursor_inactive_color"     -> "cursor.inactive.color",
      "cursor_info_bar"           -> "cursor.info_bar",
      "cursor_info_bar_placement" -> "cursor.info_bar.placement"
    )

    val modeKeys: Set[String] = Set("cursor.mode", "cursor_mode")

    val activeColorKeys: Set[String] = Set("cursor.active.color", "cursor_active_color")

    val inactiveColorKeys: Set[String] = Set("cursor.inactive.color", "cursor_inactive_color")

    val infoBarModeKeys: Set[String] = Set("cursor.info_bar", "cursor.info.bar", "cursor_info_bar")

    val infoBarPlacementKeys: Set[String] =
      Set("cursor.info_bar.placement", "cursor.info.bar.placement", "cursor_info_bar_placement")

    private val handledKeys: Set[String] =
      modeKeys ++ activeColorKeys ++ inactiveColorKeys ++ infoBarModeKeys ++ infoBarPlacementKeys

    def handles(key: String): Boolean =
      handledKeys.contains(key)

    def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val trimmed = value.trim
      if modeKeys.contains(key) then CursorMode.fromConfigKey(trimmed).map(config.withCursorMode)
      else if activeColorKeys.contains(key) then
        if trimmed.isEmpty then Some(config)
        else
          parseColor(trimmed)
            .map(color => config.withCursorColors(config.cursorColors.copy(active = Some(color))))
      else if inactiveColorKeys.contains(key) then
        if trimmed.isEmpty then Some(config)
        else
          parseColor(trimmed)
            .map(color => config.withCursorColors(config.cursorColors.copy(inactive = Some(color))))
      else if infoBarModeKeys.contains(key) then
        CursorInfoBarMode.fromConfigKey(trimmed).map(config.withCursorInfoBarMode)
      else if infoBarPlacementKeys.contains(key) then
        CursorInfoBarPlacement.fromConfigKey(trimmed).map(config.withCursorInfoBarPlacement)
      else None

    def invalidValue(key: String, value: String): Boolean =
      parse(AppConfig.default, key, value).isEmpty

    private def parseColor(value: String): Option[Color] =
      val hex = value.stripPrefix("#")
      Option
        .when(hex.length == 6 || hex.length == 8)(hex)
        .filter(_.forall(ch => Character.digit(ch, 16) >= 0))
        .flatMap { normalized =>
          scala.util.Try {
            val red   = Integer.parseInt(normalized.substring(0, 2), 16)
            val green = Integer.parseInt(normalized.substring(2, 4), 16)
            val blue  = Integer.parseInt(normalized.substring(4, 6), 16)
            val alpha = if normalized.length == 8 then Integer.parseInt(normalized.substring(6, 8), 16) else 255
            Color(red, green, blue, alpha)
          }.toOption
        }

case class EditorConfig(
    characterAnimation: Option[AnimationConfig] = AnimationConfig.none,
    fontConfig: FontConfig = FontConfig(),
    minimumPaneWidth: Int = 50
):

  def normalized: EditorConfig =
    copy(minimumPaneWidth = math.max(1, minimumPaneWidth))

object EditorConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "character.animation",
      "character.animation.duration_ms",
      "character.animation.duration.ms",
      "character.animation.steps",
      "font.code.family",
      "font.text.family",
      "font.ui.family",
      "font.code.size",
      "font.text.size",
      "font.prose.size",
      "font.ui.size",
      "font.scale.mode",
      "font.text_scale",
      "font.text.scale",
      "font.code.ligatures",
      "font.text.ligatures",
      "font.prose.ligatures",
      "font.ui.ligatures"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "character_animation"             -> "character.animation",
      "character_animation_duration_ms" -> "character.animation.duration_ms",
      "character_animation_steps"       -> "character.animation.steps",
      "font_code_family"                -> "font.code.family",
      "font_text_family"                -> "font.text.family",
      "font_ui_family"                  -> "font.ui.family",
      "font_code_size"                  -> "font.code.size",
      "font_text_size"                  -> "font.text.size",
      "font_prose_size"                 -> "font.text.size",
      "font_size"                       -> "font.code.size and font.text.size",
      "font_ui_size"                    -> "font.ui.size",
      "font_scale_mode"                 -> "font.scale.mode",
      "font_text_scale"                 -> "font.text_scale",
      "font_code_ligatures"             -> "font.code.ligatures",
      "font_text_ligatures"             -> "font.text.ligatures",
      "font_prose_ligatures"            -> "font.text.ligatures",
      "font_ligatures"                  -> "font.code.ligatures and font.text.ligatures",
      "font_ui_ligatures"               -> "font.ui.ligatures"
    )

case class DocumentConfig(
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    defaultMode: DefaultDocumentMode = DefaultDocumentMode.PlainText
)

object DocumentConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "document.markdown_view",
      "document.markdown.view",
      "document.default_mode",
      "document.default.mode"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "document_markdown_view" -> "document.markdown_view",
      "document_default_mode"  -> "document.default_mode"
    )

    val markdownViewKeys: Set[String] = Set(
      "document.markdown_view",
      "document.markdown.view",
      "document_markdown_view"
    )

    val defaultModeKeys: Set[String] = currentKeys ++ deprecatedKeys.keySet

    private val handledKeys: Set[String] = markdownViewKeys ++ defaultModeKeys

    def handles(key: String): Boolean =
      handledKeys.contains(key)

    def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val trimmed = value.trim
      if markdownViewKeys.contains(key) then MarkdownViewMode.fromConfigKey(trimmed).map(config.withMarkdownViewMode)
      else if defaultModeKeys.contains(key) then
        DefaultDocumentMode.fromConfigKey(trimmed).map(config.withDefaultDocumentMode)
      else None

    def invalidValue(key: String, value: String): Boolean =
      parse(AppConfig.default, key, value).isEmpty

case class InterfaceConfig(
    density: InterfaceDensity = InterfaceDensity.Comfortable,
    elementGap: Int = 0,
    cornerRadiusPx: Int = 8,
    outlineThicknessPx: Int = 2
):

  def normalized: InterfaceConfig =
    copy(
      elementGap = AppConfig.clampUiElementGap(elementGap),
      cornerRadiusPx = AppConfig.clampUiCornerRadiusPx(cornerRadiusPx),
      outlineThicknessPx = AppConfig.clampUiOutlineThicknessPx(outlineThicknessPx)
    )

object InterfaceConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "interface.density",
      "ui.element_gap",
      "ui.element.gap",
      "ui.corner_radius",
      "ui.corner.radius",
      "ui.outline_thickness",
      "ui.outline.thickness"
    )

    val deprecatedKeys: Map[String, String] = Map(
      "interface_density"    -> "interface.density",
      "ui_element_gap"       -> "ui.element_gap",
      "ui_corner_radius"     -> "ui.corner_radius",
      "ui_outline_thickness" -> "ui.outline_thickness"
    )

    val densityKeys: Set[String] = Set("interface.density", "interface_density")

    val elementGapKeys: Set[String] = Set("ui.element_gap", "ui.element.gap", "ui_element_gap")

    val cornerRadiusKeys: Set[String] = Set("ui.corner_radius", "ui.corner.radius", "ui_corner_radius")

    val outlineThicknessKeys: Set[String] =
      Set("ui.outline_thickness", "ui.outline.thickness", "ui_outline_thickness")

    private val handledKeys: Set[String] =
      densityKeys ++ elementGapKeys ++ cornerRadiusKeys ++ outlineThicknessKeys

    def handles(key: String): Boolean =
      handledKeys.contains(key)

    def parse(config: AppConfig, key: String, value: String): Option[AppConfig] =
      val trimmed = value.trim
      if densityKeys.contains(key) then InterfaceDensity.fromConfigKey(trimmed).map(config.withInterfaceDensity)
      else if elementGapKeys.contains(key) then
        trimmed.toIntOption
          .filter(gap => gap >= AppConfig.MinUiElementGap && gap <= AppConfig.MaxUiElementGap)
          .map(config.withUiElementGap)
      else if cornerRadiusKeys.contains(key) then
        trimmed.toIntOption
          .filter(radius => radius >= AppConfig.MinUiCornerRadiusPx && radius <= AppConfig.MaxUiCornerRadiusPx)
          .map(config.withUiCornerRadiusPx)
      else if outlineThicknessKeys.contains(key) then
        trimmed.toIntOption
          .filter(thickness =>
            thickness >= AppConfig.MinUiOutlineThicknessPx && thickness <= AppConfig.MaxUiOutlineThicknessPx
          )
          .map(config.withUiOutlineThicknessPx)
      else None

    def invalidValue(key: String, value: String): Boolean =
      parse(AppConfig.default, key, value).isEmpty

case class InputConfig(
    hotkeyConfig: HotkeyConfig = HotkeyConfig(),
    focusedKeymapConfig: FocusedKeymapConfig = FocusedKeymapConfig()
)

object InputConfig:

  object Schema:

    val dynamicPrefixes: List[String] = List(
      "hotkey.",
      "keymap.editor.",
      "keymap.command_runner.",
      "keymap.modal.",
      "keymap.panel.",
      "keymap.peek."
    )

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

case class SurfaceConfig(
    showLineNumbers: Boolean = true,
    showGutter: Boolean = true,
    wordWrapEnabled: Boolean = true,
    focusedTextBodyEnabled: Boolean = false,
    contextualToolbarEnabled: Boolean = true,
    contextualToolbarDisplayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconOnly,
    blurRadius: Float = 0.0f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    materialPreset: MaterialPreset = MaterialPreset.Frosted,
    postProcessingEffect: PostProcessingEffect = PostProcessingEffect.Off,
    motionPreset: MotionPreset = MotionPreset.Reduced,
    elementTransitionSpeedScale: Double = 1.0,
    editorTextTransitionSpeedScale: Option[Double] = None,
    commandRunnerTransitionSpeedScale: Option[Double] = None,
    uiTransitionSpeedScale: Option[Double] = None,
    cursorTransitionSpeedScale: Option[Double] = None,
    commandRunnerAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    uiAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    commandRunnerVisibleRows: Option[Int] = None,
    commandRunnerItemGapRows: Int = 0,
    commandRunnerCursorGapRows: Option[Int] = None,
    renderFpsTarget: RenderFpsTarget = RenderFpsTarget.Fps60,
    editorInsertionTransitionKind: TransitionKind = TransitionKind.Fade,
    commandRunnerTransitionKind: Option[TransitionKind] = None,
    panelOpenTransitionKind: Option[TransitionKind] = None,
    panelCloseTransitionKind: Option[TransitionKind] = None,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing()
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
      commandRunnerVisibleRows = commandRunnerVisibleRows.map(AppConfig.clampCommandRunnerVisibleRows),
      commandRunnerItemGapRows = AppConfig.clampCommandRunnerItemGapRows(commandRunnerItemGapRows),
      commandRunnerCursorGapRows = commandRunnerCursorGapRows.map(AppConfig.clampCommandRunnerCursorGapRows),
      textAreaInsets = textAreaInsets.normalized,
      viewportSizing = viewportSizing.normalized
    )

  def effectiveEditorTextTransitionSpeedScale: Double =
    editorTextTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveCommandRunnerTransitionSpeedScale: Double =
    commandRunnerTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveUiTransitionSpeedScale: Double =
    uiTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveCursorTransitionSpeedScale: Double =
    cursorTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)

  def effectiveCommandRunnerTransitionKind: TransitionKind =
    commandRunnerTransitionKind.getOrElse(TransitionKind.Fade)

  def effectivePanelOpenTransitionKind: TransitionKind =
    panelOpenTransitionKind.getOrElse(TransitionKind.OutlineThenContent)

  def effectivePanelCloseTransitionKind: TransitionKind =
    panelCloseTransitionKind.getOrElse(TransitionKind.Fade)

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

  def editorInsertionTransitionSettings: ElementTransitionSettings =
    val baseSettings = motionPreset.elementTransitionSettings
    if !baseSettings.enabled then baseSettings
    else
      baseSettings.copy(
        speedScale = effectiveEditorTextTransitionSpeedScale,
        overrides = baseSettings.overrides ++ Map(TransitionScope.EditorInsertion -> editorInsertionTransitionKind)
      )

object SurfaceConfig:

  object Schema:

    val currentKeys: Set[String] = Set(
      "ui.material",
      "material.preset",
      "ui.post_processing",
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
      "display.word_wrap",
      "display.word.wrap",
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
    )

    val deprecatedKeys: Map[String, String] = Map(
      "ui_material"                          -> "ui.material",
      "material_preset"                      -> "material.preset",
      "ui_motion"                            -> "ui.motion",
      "motion_preset"                        -> "motion.preset",
      "ui_motion_speed_scale"                -> "ui.motion.speed_scale",
      "motion_speed_scale"                   -> "motion.speed_scale",
      "ui_motion_editor_text_speed_scale"    -> "ui.motion.editor_text.speed_scale",
      "ui_motion_command_runner_speed_scale" -> "ui.motion.command_runner.speed_scale",
      "ui_motion_ui_speed_scale"             -> "ui.motion.ui.speed_scale",
      "ui_motion_cursor_speed_scale"         -> "ui.motion.cursor.speed_scale",
      "ui_motion_command_runner"             -> "ui.motion.command_runner",
      "ui_motion_command_runner_reveal"      -> "ui.motion.command_runner_reveal",
      "ui_motion_ui"                         -> "ui.motion.ui",
      "ui_motion_editor_text"                -> "ui.motion.editor_text",
      "ui_motion_panel_open"                 -> "ui.motion.panel_open",
      "ui_motion_panel_close"                -> "ui.motion.panel_close",
      "command_runner_visible_rows"          -> "command_runner.visible_rows",
      "command_runner_item_gap_rows"         -> "command_runner.item_gap_rows",
      "command_runner_cursor_gap_rows"       -> "command_runner.cursor_gap_rows",
      "render_fps"                           -> "render.fps",
      "ui_render_fps"                        -> "ui.render.fps",
      "display_word_wrap"                    -> "display.word_wrap",
      "display_focused_text_body"            -> "display.focused_text_body",
      "display_contextual_toolbar"           -> "display.contextual_toolbar",
      "display_contextual_toolbar_mode"      -> "display.contextual_toolbar_mode",
      "text_area_left_percent"               -> "text_area.left.percent",
      "text_area_right_percent"              -> "text_area.right.percent",
      "text_area_top_percent"                -> "text_area.top.percent",
      "text_area_bottom_percent"             -> "text_area.bottom.percent",
      "viewport_width_percent"               -> "viewport.width.percent",
      "viewport_width_max"                   -> "viewport.width.max",
      "viewport_height_percent"              -> "viewport.height.percent",
      "viewport_height_max"                  -> "viewport.height.max"
    )

    val commandRunnerVisibleRowsKeys: Set[String] =
      Set("command_runner.visible_rows", "command.runner.visible.rows", "command_runner_visible_rows")

    val commandRunnerItemGapRowsKeys: Set[String] =
      Set("command_runner.item_gap_rows", "command.runner.item.gap.rows", "command_runner_item_gap_rows")

    val commandRunnerCursorGapRowsKeys: Set[String] =
      Set("command_runner.cursor_gap_rows", "command.runner.cursor.gap.rows", "command_runner_cursor_gap_rows")

    val renderFpsKeys: Set[String] = Set("render.fps", "render_fps", "ui.render.fps", "ui_render_fps")

    val materialPresetKeys: Set[String] = Set("ui.material", "ui_material", "material.preset", "material_preset")

    val postProcessingKeys: Set[String] = Set("ui.post_processing")

    val motionPresetKeys: Set[String] = Set("ui.motion", "ui_motion", "motion.preset", "motion_preset")

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

    val wordWrapKeys: Set[String] = Set("display.word_wrap", "display.word.wrap", "display_word_wrap")

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
        motionPresetKeys ++
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
        renderFpsKeys ++
        wordWrapKeys ++
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
      else if motionPresetKeys.contains(key) then parseMotionPreset(trimmed).map(config.withMotionPreset)
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
      else if renderFpsKeys.contains(key) then RenderFpsTarget.fromConfigKey(trimmed).map(config.withRenderFpsTarget)
      else if wordWrapKeys.contains(key) then parseBoolean(trimmed).map(config.withWordWrap)
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
          .map(percent => config.withViewportWidthSizing(config.viewportSizing.width.copy(percent = percent)))
      else if viewportWidthMaxKeys.contains(key) then
        parseViewportMaxCells(trimmed)
          .map(maxCells => config.withViewportWidthSizing(config.viewportSizing.width.copy(maxCells = maxCells)))
      else if viewportHeightPercentKeys.contains(key) then
        parseViewportPercent(trimmed)
          .map(percent => config.withViewportHeightSizing(config.viewportSizing.height.copy(percent = percent)))
      else if viewportHeightMaxKeys.contains(key) then
        parseViewportMaxCells(trimmed)
          .map(maxCells => config.withViewportHeightSizing(config.viewportSizing.height.copy(maxCells = maxCells)))
      else None

    def invalidValue(key: String, value: String): Boolean =
      parse(AppConfig.default, key, value).isEmpty

    private def parseBoolean(value: String): Option[Boolean] =
      value.toLowerCase match
        case "true" | "on" | "enabled"    => Some(true)
        case "false" | "off" | "disabled" => Some(false)
        case _                            => None

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

    private def parseCommandRunnerItemGapRows(value: String): Option[Int] =
      value.toIntOption.filter(rows =>
        rows >= AppConfig.MinCommandRunnerItemGapRows && rows <= AppConfig.MaxCommandRunnerItemGapRows
      )

    private def parseCommandRunnerCursorGapRows(value: String): Option[Option[Int]] =
      value.toLowerCase match
        case "auto" | "default" | "" => Some(None)
        case other =>
          other.toIntOption
            .filter(rows =>
              rows >= AppConfig.MinCommandRunnerCursorGapRows && rows <= AppConfig.MaxCommandRunnerCursorGapRows
            )
            .map(rows => Some(rows))

    private def parseInsetPercent(value: String): Option[Double] =
      value.toDoubleOption
        .map(_ / 100.0)
        .filter(percent => percent >= 0.0 && percent <= TextAreaInsets.MaxInset)

    private def parseViewportPercent(value: String): Option[Double] =
      value.toDoubleOption
        .map(_ / 100.0)
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
    contextualToolbarDisplayMode: ToolbarDisplayMode = ToolbarDisplayMode.IconOnly,
    blurRadius: Float = 0.0f,
    backgroundStyle: BackgroundStyle = BackgroundStyle.Frosted,
    materialPreset: MaterialPreset = MaterialPreset.Frosted,
    postProcessingEffect: PostProcessingEffect = PostProcessingEffect.Off,
    motionPreset: MotionPreset = MotionPreset.Reduced,
    elementTransitionSpeedScale: Double = 1.0,
    editorTextTransitionSpeedScale: Option[Double] = None,
    commandRunnerTransitionSpeedScale: Option[Double] = None,
    uiTransitionSpeedScale: Option[Double] = None,
    cursorTransitionSpeedScale: Option[Double] = None,
    commandRunnerAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    uiAnimation: Option[AnimationConfig] = AnimationConfig.smooth,
    commandRunnerVisibleRows: Option[Int] = None,
    commandRunnerItemGapRows: Int = 0,
    commandRunnerCursorGapRows: Option[Int] = None,
    renderFpsTarget: RenderFpsTarget = RenderFpsTarget.Fps60,
    editorInsertionTransitionKind: TransitionKind = TransitionKind.Fade,
    commandRunnerTransitionKind: Option[TransitionKind] = None,
    panelOpenTransitionKind: Option[TransitionKind] = None,
    panelCloseTransitionKind: Option[TransitionKind] = None,
    cursorConfig: CursorConfig = CursorConfig(),
    windowConfig: WindowConfig = WindowConfig(),
    documentConfig: DocumentConfig = DocumentConfig(),
    interfaceConfig: InterfaceConfig = InterfaceConfig(),
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    viewportSizing: ViewportSizing = ViewportSizing(),
    lspUserConfig: LspUserConfig = LspUserConfig.empty,
    spellCheck: SpellCheckConfig = SpellCheckConfig()
):

  def editorConfig: EditorConfig =
    EditorConfig(
      characterAnimation = characterAnimation,
      fontConfig = fontConfig,
      minimumPaneWidth = minimumPaneWidth
    )

  def withEditorConfig(config: EditorConfig): AppConfig =
    val normalized = config.normalized
    copy(
      characterAnimation = normalized.characterAnimation,
      fontConfig = normalized.fontConfig,
      minimumPaneWidth = normalized.minimumPaneWidth
    )

  def languageToolsConfig: LanguageToolsConfig =
    LanguageToolsConfig(
      syntaxHighlightingEnabled = syntaxHighlightingEnabled,
      lspUserConfig = lspUserConfig,
      spellCheck = spellCheck
    )

  def withLanguageToolsConfig(config: LanguageToolsConfig): AppConfig =
    val normalized = config.normalized
    copy(
      syntaxHighlightingEnabled = normalized.syntaxHighlightingEnabled,
      lspUserConfig = normalized.lspUserConfig,
      spellCheck = normalized.spellCheck
    )

  def inputConfig: InputConfig =
    InputConfig(
      hotkeyConfig = hotkeyConfig,
      focusedKeymapConfig = focusedKeymapConfig
    )

  def withInputConfig(config: InputConfig): AppConfig =
    copy(
      hotkeyConfig = config.hotkeyConfig,
      focusedKeymapConfig = config.focusedKeymapConfig
    )

  def surfaceConfig: SurfaceConfig =
    SurfaceConfig(
      showLineNumbers = showLineNumbers,
      showGutter = showGutter,
      wordWrapEnabled = wordWrapEnabled,
      focusedTextBodyEnabled = focusedTextBodyEnabled,
      contextualToolbarEnabled = contextualToolbarEnabled,
      contextualToolbarDisplayMode = contextualToolbarDisplayMode,
      blurRadius = blurRadius,
      backgroundStyle = backgroundStyle,
      materialPreset = materialPreset,
      postProcessingEffect = postProcessingEffect,
      motionPreset = motionPreset,
      elementTransitionSpeedScale = elementTransitionSpeedScale,
      editorTextTransitionSpeedScale = editorTextTransitionSpeedScale,
      commandRunnerTransitionSpeedScale = commandRunnerTransitionSpeedScale,
      uiTransitionSpeedScale = uiTransitionSpeedScale,
      cursorTransitionSpeedScale = cursorTransitionSpeedScale,
      commandRunnerAnimation = commandRunnerAnimation,
      uiAnimation = uiAnimation,
      commandRunnerVisibleRows = commandRunnerVisibleRows,
      commandRunnerItemGapRows = commandRunnerItemGapRows,
      commandRunnerCursorGapRows = commandRunnerCursorGapRows,
      renderFpsTarget = renderFpsTarget,
      editorInsertionTransitionKind = editorInsertionTransitionKind,
      commandRunnerTransitionKind = commandRunnerTransitionKind,
      panelOpenTransitionKind = panelOpenTransitionKind,
      panelCloseTransitionKind = panelCloseTransitionKind,
      textAreaInsets = textAreaInsets,
      viewportSizing = viewportSizing
    )

  def withSurfaceConfig(config: SurfaceConfig): AppConfig =
    val normalized = config.normalized
    copy(
      showLineNumbers = normalized.showLineNumbers,
      showGutter = normalized.showGutter,
      wordWrapEnabled = normalized.wordWrapEnabled,
      focusedTextBodyEnabled = normalized.focusedTextBodyEnabled,
      contextualToolbarEnabled = normalized.contextualToolbarEnabled,
      contextualToolbarDisplayMode = normalized.contextualToolbarDisplayMode,
      blurRadius = normalized.blurRadius,
      backgroundStyle = normalized.backgroundStyle,
      materialPreset = normalized.materialPreset,
      postProcessingEffect = normalized.postProcessingEffect,
      motionPreset = normalized.motionPreset,
      elementTransitionSpeedScale = normalized.elementTransitionSpeedScale,
      editorTextTransitionSpeedScale = normalized.editorTextTransitionSpeedScale,
      commandRunnerTransitionSpeedScale = normalized.commandRunnerTransitionSpeedScale,
      uiTransitionSpeedScale = normalized.uiTransitionSpeedScale,
      cursorTransitionSpeedScale = normalized.cursorTransitionSpeedScale,
      commandRunnerAnimation = normalized.commandRunnerAnimation,
      uiAnimation = normalized.uiAnimation,
      commandRunnerVisibleRows = normalized.commandRunnerVisibleRows,
      commandRunnerItemGapRows = normalized.commandRunnerItemGapRows,
      commandRunnerCursorGapRows = normalized.commandRunnerCursorGapRows,
      renderFpsTarget = normalized.renderFpsTarget,
      editorInsertionTransitionKind = normalized.editorInsertionTransitionKind,
      commandRunnerTransitionKind = normalized.commandRunnerTransitionKind,
      panelOpenTransitionKind = normalized.panelOpenTransitionKind,
      panelCloseTransitionKind = normalized.panelCloseTransitionKind,
      textAreaInsets = normalized.textAreaInsets,
      viewportSizing = normalized.viewportSizing
    )

  def windowChromeMode: WindowChromeMode =
    windowConfig.chromeMode

  def preferredWindowSize: Option[PreferredWindowSize] =
    windowConfig.preferredSize

  def markdownViewMode: MarkdownViewMode =
    documentConfig.markdownViewMode

  def defaultDocumentMode: DefaultDocumentMode =
    documentConfig.defaultMode

  def interfaceDensity: InterfaceDensity =
    interfaceConfig.density

  def uiElementGap: Int =
    interfaceConfig.elementGap

  def uiCornerRadiusPx: Int =
    interfaceConfig.cornerRadiusPx

  def uiOutlineThicknessPx: Int =
    interfaceConfig.outlineThicknessPx

  /** Create a new config with character animation enabled */
  def withCharacterAnimation(config: AnimationConfig): AppConfig =
    withEditorConfig(editorConfig.copy(characterAnimation = Some(config)))
      .withSurfaceConfig(surfaceConfig.copy(motionPreset = MotionPreset.Custom))

  /** Create a new config with character animation disabled */
  def withoutCharacterAnimation: AppConfig =
    withEditorConfig(editorConfig.copy(characterAnimation = None)).withSurfaceConfig(
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

  /** Create a new config with syntax highlighting toggled */
  def withSyntaxHighlighting(enabled: Boolean): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(syntaxHighlightingEnabled = enabled))

  def withHotkeyConfig(config: HotkeyConfig): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = config))

  def withHotkeyOverride(action: HotkeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = hotkeyConfig.withBinding(action, binding)))

  def resetHotkeyOverride(action: HotkeyAction): AppConfig =
    withInputConfig(inputConfig.copy(hotkeyConfig = hotkeyConfig.resetBinding(action)))

  def withFocusedKeymapConfig(config: FocusedKeymapConfig): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = config))

  def withEditorKeyOverride(action: EditorKeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.withEditorBinding(action, binding)))

  def resetEditorKeyOverride(action: EditorKeyAction): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.resetEditorBinding(action)))

  def withCommandRunnerKeyOverride(action: CommandRunnerKeyAction, binding: String): AppConfig =
    withInputConfig(
      inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.withCommandRunnerBinding(action, binding))
    )

  def resetCommandRunnerKeyOverride(action: CommandRunnerKeyAction): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.resetCommandRunnerBinding(action)))

  def withModalKeyOverride(action: ModalKeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.withModalBinding(action, binding)))

  def resetModalKeyOverride(action: ModalKeyAction): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.resetModalBinding(action)))

  def withPanelKeyOverride(action: PanelKeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.withPanelBinding(action, binding)))

  def resetPanelKeyOverride(action: PanelKeyAction): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.resetPanelBinding(action)))

  def withPeekKeyOverride(action: PeekKeyAction, binding: String): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.withPeekBinding(action, binding)))

  def resetPeekKeyOverride(action: PeekKeyAction): AppConfig =
    withInputConfig(inputConfig.copy(focusedKeymapConfig = focusedKeymapConfig.resetPeekBinding(action)))

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

  /** Create a new config with word wrapping toggled */
  def withWordWrap(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(wordWrapEnabled = enabled))

  def withFocusedTextBody(enabled: Boolean): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(focusedTextBodyEnabled = enabled))

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

  def withMotionPreset(preset: MotionPreset): AppConfig =
    preset match
      case MotionPreset.Custom =>
        withSurfaceConfig(surfaceConfig.copy(motionPreset = MotionPreset.Custom))
      case _ =>
        copy(characterAnimation = preset.animationConfig).withSurfaceConfig(
          surfaceConfig.copy(
            motionPreset = preset,
            commandRunnerAnimation = preset.animationConfig,
            uiAnimation = preset.animationConfig
          )
        )

  /** Transition policy derived from the selected motion preset and UI speed scale. */
  def elementTransitionSettings: ElementTransitionSettings =
    surfaceConfig.elementTransitionSettings

  /** Transition policy derived from the selected motion preset and editor text speed scale. */
  def editorInsertionTransitionSettings: ElementTransitionSettings =
    surfaceConfig.editorInsertionTransitionSettings

  def withElementTransitionSpeedScale(scale: Double): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(elementTransitionSpeedScale = scale))

  def withEditorTextTransitionSpeedScale(scale: Option[Double]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(editorTextTransitionSpeedScale = scale))

  def withCommandRunnerTransitionSpeedScale(scale: Option[Double]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerTransitionSpeedScale = scale))

  def withUiTransitionSpeedScale(scale: Option[Double]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(uiTransitionSpeedScale = scale))

  def withCursorTransitionSpeedScale(scale: Option[Double]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(cursorTransitionSpeedScale = scale))

  def withCommandRunnerAnimation(animation: Option[AnimationConfig]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerAnimation = animation))

  def withUiAnimation(animation: Option[AnimationConfig]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(uiAnimation = animation))

  def withCommandRunnerVisibleRows(rows: Option[Int]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerVisibleRows = rows))

  def withCommandRunnerItemGapRows(rows: Int): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerItemGapRows = rows))

  def withCommandRunnerCursorGapRows(rows: Option[Int]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerCursorGapRows = rows))

  def withRenderFpsTarget(target: RenderFpsTarget): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(renderFpsTarget = target))

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
    AppConfig.scaledAnimation(characterAnimation, effectiveEditorTextTransitionSpeedScale)

  /** Command runner animation after applying the effective command runner motion speed. */
  def scaledCommandRunnerAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(commandRunnerAnimation, effectiveCommandRunnerTransitionSpeedScale)

  /** General UI animation after applying the effective UI motion speed. */
  def scaledUiAnimation: Option[AnimationConfig] =
    AppConfig.scaledAnimation(uiAnimation, effectiveUiTransitionSpeedScale)

  def withEditorInsertionTransitionKind(kind: TransitionKind): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(editorInsertionTransitionKind = kind))

  def withCommandRunnerTransitionKind(kind: Option[TransitionKind]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(commandRunnerTransitionKind = kind))

  def effectiveCommandRunnerTransitionKind: TransitionKind =
    surfaceConfig.effectiveCommandRunnerTransitionKind

  def withPanelOpenTransitionKind(kind: Option[TransitionKind]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(panelOpenTransitionKind = kind))

  def withPanelCloseTransitionKind(kind: Option[TransitionKind]): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(panelCloseTransitionKind = kind))

  def effectivePanelOpenTransitionKind: TransitionKind =
    surfaceConfig.effectivePanelOpenTransitionKind

  def effectivePanelCloseTransitionKind: TransitionKind =
    surfaceConfig.effectivePanelCloseTransitionKind

  def cursorMode: CursorMode =
    cursorConfig.mode

  def cursorColors: CursorColorConfig =
    cursorConfig.colors

  def cursorInfoBarMode: CursorInfoBarMode =
    cursorConfig.infoBarMode

  def cursorInfoBarPlacement: CursorInfoBarPlacement =
    cursorConfig.infoBarPlacement

  def withCursorConfig(config: CursorConfig): AppConfig =
    copy(cursorConfig = config)

  def withCursorMode(mode: CursorMode): AppConfig =
    withCursorConfig(cursorConfig.copy(mode = mode))

  def withCursorColors(colors: CursorColorConfig): AppConfig =
    withCursorConfig(cursorConfig.copy(colors = colors))

  def withCursorInfoBarMode(mode: CursorInfoBarMode): AppConfig =
    withCursorConfig(cursorConfig.copy(infoBarMode = mode))

  def withCursorInfoBarPlacement(placement: CursorInfoBarPlacement): AppConfig =
    withCursorConfig(cursorConfig.copy(infoBarPlacement = placement))

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

  def withInterfaceConfig(config: InterfaceConfig): AppConfig =
    copy(interfaceConfig = config.normalized)

  def withInterfaceDensity(density: InterfaceDensity): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(density = density))

  def withUiElementGap(gap: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(elementGap = gap))

  def withUiCornerRadiusPx(radius: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(cornerRadiusPx = radius))

  def withUiOutlineThicknessPx(thickness: Int): AppConfig =
    withInterfaceConfig(interfaceConfig.copy(outlineThicknessPx = thickness))

  def withTextAreaInsets(insets: TextAreaInsets): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(textAreaInsets = insets))

  def withTextAreaLeftInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(left = value))

  def withTextAreaRightInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(right = value))

  def withTextAreaTopInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(top = value))

  def withTextAreaBottomInset(value: Double): AppConfig =
    withTextAreaInsets(textAreaInsets.copy(bottom = value))

  def withViewportSizing(sizing: ViewportSizing): AppConfig =
    withSurfaceConfig(surfaceConfig.copy(viewportSizing = sizing))

  def withViewportWidthSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(viewportSizing.copy(width = sizing))

  def withViewportHeightSizing(sizing: ViewportAxisSizing): AppConfig =
    withViewportSizing(viewportSizing.copy(height = sizing))

  def withPreferredWindowSize(size: PreferredWindowSize): AppConfig =
    withWindowConfig(windowConfig.copy(preferredSize = Some(size.normalized)))

  def withLspUserConfig(config: LspUserConfig): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(lspUserConfig = config))

  def withSpellCheck(config: SpellCheckConfig): AppConfig =
    withLanguageToolsConfig(languageToolsConfig.copy(spellCheck = config))

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
  val MinCommandRunnerItemGapRows: Int       = 0
  val MaxCommandRunnerItemGapRows: Int       = 8
  val MinCommandRunnerCursorGapRows: Int     = 0
  val MaxCommandRunnerCursorGapRows: Int     = 8

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

  def clampCommandRunnerItemGapRows(rows: Int): Int =
    rows.max(MinCommandRunnerItemGapRows).min(MaxCommandRunnerItemGapRows)

  def clampCommandRunnerCursorGapRows(rows: Int): Int =
    rows.max(MinCommandRunnerCursorGapRows).min(MaxCommandRunnerCursorGapRows)

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
