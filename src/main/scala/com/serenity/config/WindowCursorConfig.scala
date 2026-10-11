package com.serenity.config

import java.awt.Color

import com.serenity.ui.fonts.FontLoader.FontConfig

final case class PreferredWindowSize(width: Int, height: Int):
  def normalized: PreferredWindowSize =
    PreferredWindowSize(width.max(400), height.max(300))

object PreferredWindowSize:
  /** The size of the window when the config states none. */
  val Default: PreferredWindowSize = PreferredWindowSize(1024, 768)

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

/** `blinkTimeoutMillis` is how long a focused caret keeps blinking without input before it holds solid, so an idle
  * editor stops waking to redraw it (#1883, after GTK's `gtk-cursor-blink-timeout`); 0 blinks for as long as the window
  * stays focused.
  */
final case class CursorConfig(
    mode: CursorMode = CursorMode.Blink,
    colors: CursorColorConfig = CursorColorConfig(),
    blinkTimeoutMillis: Long = 10000L
)

final case class EditorConfig(
    fontConfig: FontConfig = FontConfig(),
    minimumPaneWidth: Int = 50
):

  def normalized: EditorConfig =
    copy(minimumPaneWidth = math.max(1, minimumPaneWidth))

final case class DocumentConfig(
    markdownViewMode: MarkdownViewMode = MarkdownViewMode.Source,
    defaultMode: DefaultDocumentMode = DefaultDocumentMode.PlainText,
    // A target word count for the active document; `None` means no goal is set. Progress is shown by the
    // `StatusSegment.WordGoal` status-line segment as the document's total word count against this target -- not a
    // daily-delta tracker, since that would need date-based session state this config has no home for yet.
    wordGoal: Option[Int] = None,
    // Gates the multi-line drop cap paragraph role (issue: "Drop caps"). When false, a paragraph already tagged
    // ParagraphRole.DropCap keeps that role in the document (no data loss), but rendering/layout treats it as plain
    // Body -- see RichTextStyling/TextLayoutSnapshot's drop-cap gating.
    dropCapsEnabled: Boolean = true,
    // Who new comments and replies are written as; `None` means the operating system's user name (`CommentAuthor`).
    commentAuthor: Option[String] = None,
    // Whether a chapter note's overview is painted, faded, under an empty chapter when a session starts; the
    // Toggle Chapter Ghosts command flips it for the session.
    chapterGhosts: Boolean = true,
    // How long edits must pause before the whole-document spell-check analysis and the outline re-parse run. Longer
    // waits spend less CPU while typing at the price of staler squiggles and outline.
    analysisDebounceMillis: Long = 150L,
    outlineDebounceMillis: Long = 150L
)

final case class AppModeConfig(
    mode: AppMode = AppMode.Code,
    showAllSettingsRegardlessOfMode: Boolean = false
)
