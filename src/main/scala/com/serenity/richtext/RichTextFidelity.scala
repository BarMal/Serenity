package com.serenity.richtext

/** Signals that saving an imported rich document would discard source content. */
final class LossyRichTextOverwriteException(message: String) extends RuntimeException(message)

/** Details about content that a format adapter cannot represent in Serenity's model. */
final case class RichTextFidelity(
    unsupportedElements: Set[String] = Set.empty,
    unsupportedArchiveEntries: Set[String] = Set.empty
):
  /** Whether saving the imported document can preserve all inspected source content. */
  def isLossless: Boolean = unsupportedElements.isEmpty && unsupportedArchiveEntries.isEmpty

/** A decoded document together with the fidelity decision made during import. */
final case class RichTextImport(document: RichTextDocument, fidelity: RichTextFidelity)
