package com.serenity.richtext

/** Details about content that a format adapter cannot represent in Serenity's model. */
case class RichTextFidelity(
    unsupportedElements: Set[String] = Set.empty,
    unsupportedArchiveEntries: Set[String] = Set.empty
):
  /** Whether saving the imported document can preserve all inspected source content. */
  def isLossless: Boolean = unsupportedElements.isEmpty && unsupportedArchiveEntries.isEmpty

/** A decoded document together with the fidelity decision made during import. */
case class RichTextImport(document: RichTextDocument, fidelity: RichTextFidelity)
