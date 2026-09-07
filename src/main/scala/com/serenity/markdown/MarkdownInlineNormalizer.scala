package com.serenity.markdown

/** Converts inline Markdown syntax (images, links, inline code) into flat display text for the inline Lens. Shared by
  * [[MarkdownDocumentPreview]]'s line-level rendering and [[MarkdownInlineTablePreview]]'s cell rendering, so it lives
  * on its own rather than being owned by (and reached into from) either one.
  */
private[markdown] object MarkdownInlineNormalizer:

  def normalizeInline(text: String): String =
    val withoutImages = """!\[([^\]]*)\]\(([^)]+)\)""".r.replaceAllIn(
      text,
      matched =>
        val label = Option(matched.group(1).trim).filter(_.nonEmpty).getOrElse("Image")
        s"Image: $label (${matched.group(2)})"
    )
    val withoutLinks = """\[([^\]]+)\]\(([^)]+)\)""".r.replaceAllIn(
      withoutImages,
      matched => s"${matched.group(1)} (${matched.group(2)})"
    )
    "`([^`]+)`".r.replaceAllIn(withoutLinks, matched => matched.group(1))
