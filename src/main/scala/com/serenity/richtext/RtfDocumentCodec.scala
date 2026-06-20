package com.serenity.richtext

import java.awt.Color
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import javax.swing.text as SwingText
import javax.swing.text.rtf.RTFEditorKit

import cats.effect.IO

/** Reads and writes RTF documents through Serenity's native rich text model. */
object RtfDocumentCodec:
  /** Read an RTF file into Serenity's native rich text model. */
  def read(path: Path): IO[RichTextDocument] =
    IO.blocking(readBytes(Files.readAllBytes(path)))

  /** Write Serenity's native rich text model to an RTF file. */
  def write(document: RichTextDocument, path: Path): IO[Unit] =
    IO.blocking(Files.write(path, writeBytes(document))).void

  /** Decode RTF bytes into Serenity's native rich text model. */
  def readBytes(bytes: Array[Byte]): RichTextDocument =
    val styledDocument = SwingText.DefaultStyledDocument()
    val input          = ByteArrayInputStream(bytes)
    try RTFEditorKit().read(input, styledDocument, 0)
    finally input.close()
    fromStyledDocument(styledDocument)

  /** Encode Serenity's native rich text model as RTF bytes. */
  def writeBytes(document: RichTextDocument): Array[Byte] =
    val styledDocument = toStyledDocument(document.normalized)
    val output         = ByteArrayOutputStream()
    RTFEditorKit().write(output, styledDocument, 0, styledDocument.getLength)
    output.toByteArray

  private def fromStyledDocument(document: SwingText.StyledDocument): RichTextDocument =
    val root = document.getDefaultRootElement
    val paragraphs = (0 until root.getElementCount).toList.flatMap { index =>
      val element = root.getElement(index)
      val paragraph = RichTextParagraph(
        runs = runsFromElement(document, element),
        alignment = alignmentFromAttributes(element.getAttributes)
      ).normalized

      Option.when(paragraph.plainText.nonEmpty)(paragraph)
    }

    RichTextDocument(
      if paragraphs.nonEmpty then paragraphs
      else List(RichTextParagraph.plain(""))
    ).normalized

  private def runsFromElement(
    document: SwingText.StyledDocument,
    paragraphElement: SwingText.Element
  ): List[RichTextRun] =
    (0 until paragraphElement.getElementCount).toList.flatMap { index =>
      val element = paragraphElement.getElement(index)
      val start   = element.getStartOffset.max(paragraphElement.getStartOffset)
      val end     = element.getEndOffset.min(paragraphElement.getEndOffset).min(document.getLength)
      val length  = (end - start).max(0)

      Option
        .when(length > 0) {
          val text = document
            .getText(start, length)
            .replace("\r", "")
            .stripSuffix("\n")
          Option.when(text.nonEmpty)(RichTextRun(text, styleFromAttributes(element.getAttributes)))
        }
        .flatten
    }

  private def toStyledDocument(document: RichTextDocument): SwingText.StyledDocument =
    val styledDocument = SwingText.DefaultStyledDocument()

    document.paragraphs.zipWithIndex.foreach { (paragraph, index) =>
      val paragraphStart = styledDocument.getLength
      paragraph.runs.foreach { run =>
        styledDocument.insertString(
          styledDocument.getLength,
          run.text,
          attributesFromStyle(run.style)
        )
      }

      if index < document.paragraphs.size - 1 then
        styledDocument.insertString(styledDocument.getLength, "\n", SwingText.SimpleAttributeSet())

      val paragraphLength = (styledDocument.getLength - paragraphStart).max(1)
      styledDocument.setParagraphAttributes(
        paragraphStart,
        paragraphLength,
        attributesFromAlignment(paragraph.alignment),
        false
      )
    }

    styledDocument

  private def styleFromAttributes(attributes: SwingText.AttributeSet): RichTextStyle =
    RichTextStyle(
      marks = List(
        Option.when(SwingText.StyleConstants.isBold(attributes))(InlineMark.Bold),
        Option.when(SwingText.StyleConstants.isItalic(attributes))(InlineMark.Italic),
        Option.when(SwingText.StyleConstants.isUnderline(attributes))(InlineMark.Underline)
      ).flatten.toSet,
      fontFamily = Option
        .when(attributes.isDefined(SwingText.StyleConstants.FontFamily))(
          SwingText.StyleConstants.getFontFamily(attributes)
        )
        .filter(_.nonEmpty),
      fontSize = Option
        .when(attributes.isDefined(SwingText.StyleConstants.FontSize))(
          SwingText.StyleConstants.getFontSize(attributes).toFloat
        )
        .filter(_ > 0),
      color = Option
        .when(attributes.isDefined(SwingText.StyleConstants.Foreground))(
          SwingText.StyleConstants.getForeground(attributes)
        )
        .flatMap(colorToHex)
    )

  private def attributesFromStyle(style: RichTextStyle): SwingText.AttributeSet =
    val attributes = SwingText.SimpleAttributeSet()
    SwingText.StyleConstants.setBold(attributes, style.marks.contains(InlineMark.Bold))
    SwingText.StyleConstants.setItalic(attributes, style.marks.contains(InlineMark.Italic))
    SwingText.StyleConstants.setUnderline(attributes, style.marks.contains(InlineMark.Underline))
    style.fontFamily.foreach(SwingText.StyleConstants.setFontFamily(attributes, _))
    style.fontSize.foreach(size => SwingText.StyleConstants.setFontSize(attributes, size.round))
    style.color.flatMap(hexToColor).foreach(SwingText.StyleConstants.setForeground(attributes, _))
    attributes

  private def alignmentFromAttributes(attributes: SwingText.AttributeSet): ParagraphAlignment =
    SwingText.StyleConstants.getAlignment(attributes) match
      case SwingText.StyleConstants.ALIGN_CENTER    => ParagraphAlignment.Center
      case SwingText.StyleConstants.ALIGN_RIGHT     => ParagraphAlignment.Right
      case SwingText.StyleConstants.ALIGN_JUSTIFIED => ParagraphAlignment.Justify
      case _                                        => ParagraphAlignment.Left

  private def attributesFromAlignment(alignment: ParagraphAlignment): SwingText.AttributeSet =
    val attributes = SwingText.SimpleAttributeSet()
    val swingAlignment = alignment match
      case ParagraphAlignment.Left    => SwingText.StyleConstants.ALIGN_LEFT
      case ParagraphAlignment.Center  => SwingText.StyleConstants.ALIGN_CENTER
      case ParagraphAlignment.Right   => SwingText.StyleConstants.ALIGN_RIGHT
      case ParagraphAlignment.Justify => SwingText.StyleConstants.ALIGN_JUSTIFIED
    SwingText.StyleConstants.setAlignment(attributes, swingAlignment)
    attributes

  private def colorToHex(color: Color): Option[String] =
    Option(color)
      .filterNot(_ == Color.BLACK)
      .map(color => f"#${color.getRed}%02x${color.getGreen}%02x${color.getBlue}%02x")

  private def hexToColor(value: String): Option[Color] =
    val normalized = value.stripPrefix("#")
    Option.when(normalized.matches("[0-9a-fA-F]{6}")) {
      Color(
        Integer.parseInt(normalized.substring(0, 2), 16),
        Integer.parseInt(normalized.substring(2, 4), 16),
        Integer.parseInt(normalized.substring(4, 6), 16)
      )
    }
