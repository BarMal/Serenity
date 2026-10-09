package com.serenity.richtext

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipEntry, ZipInputStream}

/** Reading packages back for specs: what is in them, in what order, and how each entry is stored. */
object RichTextTestPackages:
  final case class Entry(name: String, bytes: Seq[Byte], method: Int):
    def text: String = String(bytes.toArray, StandardCharsets.UTF_8)

  def entries(archive: Array[Byte]): List[Entry] =
    val input = ZipInputStream(ByteArrayInputStream(archive))
    try
      Iterator
        .continually(input.getNextEntry)
        .takeWhile(_ != null)
        .map(entry => Entry(entry.getName, input.readAllBytes().toSeq, entry.getMethod))
        .toList
    finally input.close()

  def entry(archive: Array[Byte], name: String): Entry =
    entries(archive).find(_.name == name).getOrElse(throw AssertionError(s"Missing package entry: $name"))

  def names(archive: Array[Byte]): List[String] =
    entries(archive).map(_.name)

  /** Replaces the first occurrence of `find` in paragraph `index` with `replacement`, through the document's own edit.
    */
  def replaceText(document: RichTextDocument, index: Int, find: String, replacement: String): RichTextDocument =
    val text = document.paragraphAt(index).map(_.plainText).getOrElse("")
    val from = text.indexOf(find)
    if from < 0 then throw AssertionError(s"'$find' is not in paragraph $index: '$text'")
    document
      .replaceRange(
        RichTextRange(RichTextPosition(index, from), RichTextPosition(index, from + find.length)),
        replacement
      )
      .normalized

  val ZipStored: Int = ZipEntry.STORED
