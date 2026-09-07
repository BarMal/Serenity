package com.serenity.config

import com.serenity.animation.*

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

/** A window corner the mode/tab widget can be addressed to (issue #1307). Distinct from `CursorInfoBarPlacement`, which
  * only chooses between a floating overlay and folding into the full-width bottom bar -- this widget is always its own
  * small corner element, so it needs an actual corner, not an on/off-bar toggle.
  */
enum CornerPosition(val configKey: String):
  case TopLeft     extends CornerPosition("top-left")
  case TopRight    extends CornerPosition("top-right")
  case BottomLeft  extends CornerPosition("bottom-left")
  case BottomRight extends CornerPosition("bottom-right")

object CornerPosition:

  def fromConfigKey(value: String): Option[CornerPosition] =
    value.trim.toLowerCase match
      case "top-left"     => Some(CornerPosition.TopLeft)
      case "top-right"    => Some(CornerPosition.TopRight)
      case "bottom-left"  => Some(CornerPosition.BottomLeft)
      case "bottom-right" => Some(CornerPosition.BottomRight)
      case _              => None
