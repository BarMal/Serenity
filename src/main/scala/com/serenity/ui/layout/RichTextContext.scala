package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.Buffer
import com.serenity.ui.theme.RichTextStyling

/** The rich-text inputs a logical line's wrap depends on besides its text: the paragraph styles that choose each run's
  * font and a drop cap's role, the prose zoom that scales them, and whether drop caps are enabled at all.
  */
final case class RichTextContext(document: Option[RichTextDocument], proseScale: Float, dropCapsEnabled: Boolean)

object RichTextContext:

  /** A buffer with no rich-text styling: every line wraps in the one base font. */
  val plain: RichTextContext = RichTextContext(None, 1.0f, dropCapsEnabled = true)

  /** The context the renderer paints `buffer` with at `font`, so row counts can follow the painted wrap. A document
    * that no longer has the buffer's text shape is stale and ignored, exactly as the painted layout ignores it.
    */
  def forBuffer(buffer: Buffer, font: Font, dropCapsEnabled: Boolean): RichTextContext =
    val content = buffer.document.content
    RichTextContext(
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(content.lineCount, content.weight)),
      RichTextStyling.proseZoom(font.getSize2D),
      dropCapsEnabled
    )
