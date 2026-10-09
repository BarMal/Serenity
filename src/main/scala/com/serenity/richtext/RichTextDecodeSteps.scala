package com.serenity.richtext

import org.w3c.dom.Document as XmlDocument

/** The two expensive steps of opening a DOCX or ODT, injectable so a spec can count them. */
final private[richtext] case class RichTextDecodeSteps(
    readArchive: (Array[Byte], String, Set[String]) => ArchiveContents,
    parseBody: Array[Byte] => XmlDocument
)

private[richtext] object RichTextDecodeSteps:
  val live: RichTextDecodeSteps = RichTextDecodeSteps(RichTextArchive.read, RichTextXmlParser.parse)
