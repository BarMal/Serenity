package com.serenity.manuscript.epub

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDateTime}
import java.util.zip.{CRC32, ZipEntry, ZipOutputStream}

import com.serenity.manuscript.Manuscript

/** Writes a [[Manuscript]] as an EPUB 3 e-book: reflowable XHTML, one file per chapter, so the reader sets the type.
  *
  * The archive is byte-for-byte deterministic for a given manuscript and `modified` time: fixed entry order and
  * timestamps, and an identifier derived from the title and author unless the manuscript configures one. `modified` is
  * a parameter because EPUB requires a `dcterms:modified` stamp and a pure writer cannot read the clock.
  */
object ManuscriptEpubWriter:

  val MediaType: String = "application/epub+zip"

  /** The first DOS time after the epoch, so the archive never records when it was written. Exactly midnight on 1
    * January 1980 is the JDK's "before 1980" marker and would add an extended-timestamp extra field, which EPUB forbids
    * on `mimetype`.
    */
  private val EntryTime = LocalDateTime.of(1980, 1, 1, 0, 0, 2)

  def write(manuscript: Manuscript, modified: Instant): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      putStored(zip, "mimetype", MediaType.getBytes(StandardCharsets.US_ASCII))
      parts(manuscript, modified).foreach((name, content) => putDeflated(zip, name, content))
    finally zip.close()
    output.toByteArray

  /** Every part of the package after `mimetype`, in archive order. */
  def parts(manuscript: Manuscript, modified: Instant): List[(String, String)] =
    val content = EpubDocuments.of(manuscript)
    val meta    = manuscript.meta
    List(
      "META-INF/container.xml" -> EpubPackage.container,
      EpubPackage.ContentPath  -> EpubPackage.packageDocument(meta, content, modified),
      "OEBPS/nav.xhtml"        -> EpubPackage.navigation(meta, content),
      "OEBPS/css/style.css"    -> EpubPackage.stylesheet
    ) ++ content.all.map(document => s"OEBPS/${document.path}" -> EpubPackage.page(meta, document))

  /** The EPUB container rules require `mimetype` first, uncompressed, and without extra fields. */
  private def putStored(zip: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    val checksum = CRC32()
    checksum.update(bytes)
    val entry = ZipEntry(name)
    entry.setMethod(ZipEntry.STORED)
    entry.setSize(bytes.length.toLong)
    entry.setCompressedSize(bytes.length.toLong)
    entry.setCrc(checksum.getValue)
    entry.setTimeLocal(EntryTime)
    zip.putNextEntry(entry)
    zip.write(bytes)
    zip.closeEntry()

  private def putDeflated(zip: ZipOutputStream, name: String, content: String): Unit =
    val entry = ZipEntry(name)
    entry.setTimeLocal(EntryTime)
    zip.putNextEntry(entry)
    zip.write(content.getBytes(StandardCharsets.UTF_8))
    zip.closeEntry()
