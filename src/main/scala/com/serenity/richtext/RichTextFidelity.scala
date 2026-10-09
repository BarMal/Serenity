package com.serenity.richtext

/** Signals that saving an imported rich document would discard source content. */
final class LossyRichTextOverwriteException(message: String) extends RuntimeException(message)

/** A decoded document together with what saving it back over the file it came from would do. */
final case class RichTextImport(document: RichTextDocument, fidelity: FidelityReport)
