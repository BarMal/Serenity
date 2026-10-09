package com.serenity.richtext

import com.serenity.richtext.DocxProperties.WNs
import com.serenity.richtext.OdtStyles.{OfficeNs, TextNs}
import org.w3c.dom.Element

/** Decides what each child of a document body is in the model, and builds the read-only line for a block. */
private[richtext] object BodyBlocks:
  private val TableNs   = "urn:oasis:names:tc:opendocument:xmlns:table:1.0"
  private val DrawingNs = "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
  private val MathNs    = "http://schemas.openxmlformats.org/officeDocument/2006/math"

  private val DocxStructural = Set(
    "sectPr",
    "bookmarkStart",
    "bookmarkEnd",
    "commentRangeStart",
    "commentRangeEnd",
    "permStart",
    "permEnd",
    "proofErr",
    "moveFromRangeStart",
    "moveFromRangeEnd",
    "moveToRangeStart",
    "moveToRangeEnd"
  )

  private val OdtStructural = Set(
    (TextNs, "sequence-decls"),
    (TextNs, "variable-decls"),
    (TextNs, "user-field-decls"),
    (TextNs, "dde-connection-decls"),
    (TextNs, "alphabetical-index-auto-mark-file"),
    (TextNs, "tracked-changes"),
    (TextNs, "change"),
    (TextNs, "change-start"),
    (TextNs, "change-end"),
    (TableNs, "calculation-settings"),
    (OfficeNs, "forms")
  )

  private val OdtIndexes = Set(
    "table-of-content",
    "alphabetical-index",
    "bibliography",
    "illustration-index",
    "object-index",
    "table-index",
    "user-index"
  )

  def docx(element: Element): BodyKind =
    val name = element.getLocalName
    if element.getNamespaceURI == MathNs && (name == "oMathPara" || name == "oMath") then
      BodyKind.Block(DocumentFeature.Equations)
    else if element.getNamespaceURI == WNs then
      name match
        case "p"                       => BodyKind.Paragraph
        case _ if DocxStructural(name) => BodyKind.Structural
        case "tbl"                     => BodyKind.Block(DocumentFeature.Tables)
        case "sdt"                     => BodyKind.Block(DocumentFeature.ContentControls)
        case "altChunk"                => BodyKind.Block(DocumentFeature.Embedded)
        case "ins" | "del" | "moveFrom" | "moveTo" =>
          BodyKind.Block(DocumentFeature.TrackedChanges)
        case other => BodyKind.Block(DocumentFeature.Other(other))
    else BodyKind.Block(DocumentFeature.Other(name))

  def odt(element: Element): BodyKind =
    val namespace = element.getNamespaceURI
    val name      = element.getLocalName
    if namespace == TextNs && (name == "p" || name == "h") then BodyKind.Paragraph
    else if OdtStructural.contains((namespace, name)) then BodyKind.Structural
    else if namespace == TableNs && name == "table" then BodyKind.Block(DocumentFeature.Tables)
    else if namespace == TextNs && (name == "list" || name == "numbered-paragraph") then
      BodyKind.Block(DocumentFeature.Lists)
    else if namespace == TextNs && name == "section" then BodyKind.Block(DocumentFeature.Sections)
    else if namespace == TextNs && OdtIndexes(name) then BodyKind.Block(DocumentFeature.Indexes)
    else if namespace == DrawingNs then BodyKind.Block(DocumentFeature.Images)
    else BodyKind.Block(DocumentFeature.Other(name))

  /** The read-only line for the body child at `index`. It has a source, and so is copied from the package untouched,
    * only when the text of the part could be mapped; otherwise `raw` is empty and the block cannot be written back.
    */
  def line(raw: Option[String], feature: DocumentFeature, index: Int): RichTextParagraph =
    val imported = RichTextParagraph.block(raw.getOrElse(""), feature)
    raw.fold(imported)(_ => imported.copy(source = Some(ParagraphSource(index, "", Nil, Some(imported)))))
