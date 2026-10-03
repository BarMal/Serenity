package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Everything a paint step needs that is neither `AppState` nor scene geometry: the surface being painted, the resolved
  * layout, the three fonts and their metrics, the per-frame cursor state, and this frame's owner-scoped render caches.
  * Threaded through every renderer in this package, so it lives on its own rather than belonging to any one of them.
  *
  * `caches` defaults to a freshly constructed [[com.serenity.state.manager.RenderCaches]] only so existing test
  * fixtures that build a `RenderContext` directly (and don't care about cache identity/sharing) keep compiling without
  * naming one explicitly -- every production render entry point supplies its owning [[StateManager]]'s single instance
  * instead (issue #1677), never this default.
  */
final case class RenderContext(
    surface: RenderSurface,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true,
    cursorColorOverride: Option[java.awt.Color] = None,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    caches: com.serenity.state.manager.RenderCaches = com.serenity.state.manager.RenderCaches.create()
):

  def fontForRole(role: TypographyRole): java.awt.Font =
    role match
      case TypographyRole.Code            => codeFont
      case TypographyRole.Prose           => textFont
      case TypographyRole.MarkdownSource  => textFont
      case TypographyRole.MarkdownPreview => textFont
      case TypographyRole.Ui              => uiFont
      case TypographyRole.Mixed           => textFont

  def fontForBuffer(buffer: Buffer): java.awt.Font =
    fontForRole(buffer.typographyRole)
