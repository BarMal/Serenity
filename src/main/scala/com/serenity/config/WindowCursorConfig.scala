package com.serenity.config

import java.awt.Color

import com.serenity.animation.AnimationConfig
import com.serenity.ui.fonts.FontLoader.FontConfig

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

final case class CursorConfig(
    mode: CursorMode = CursorMode.Blink,
    colors: CursorColorConfig = CursorColorConfig()
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
