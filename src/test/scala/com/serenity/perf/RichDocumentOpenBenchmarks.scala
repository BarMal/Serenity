package com.serenity.perf

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.util.Random

import com.serenity.richtext.{
  DocxDocumentCodec,
  InlineMark,
  OdtDocumentCodec,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}

/** Cold-path cost of opening a manuscript-sized DOCX or ODT (#1882): a package of about 200 styled paragraphs plus the
  * parts a word processor always writes beside the body (styles, theme, a media file), which every extra pass over the
  * archive has to inflate again.
  *
  * Run just these with `sbt "Test / runMain com.serenity.perf.RichDocumentOpenBenchmarks"`.
  */
object RichDocumentOpenBenchmarks:

  val ParagraphCount: Int = 200

  private val Iterations = 40

  def benchmarks: List[BenchmarkRunner.Benchmark] =
    val docx = docxFixture
    val odt  = odtFixture
    List(
      openBenchmark("richdoc.open.docx", docx, DocxDocumentCodec.readBytesWithFidelity),
      openBenchmark("richdoc.open.odt", odt, OdtDocumentCodec.readBytesWithFidelity)
    )

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks))

  private def openBenchmark(
    name: String,
    bytes: Array[Byte],
    open: Array[Byte] => Either[com.serenity.richtext.RichTextCodecException, com.serenity.richtext.RichTextImport]
  ): BenchmarkRunner.Benchmark =
    BenchmarkRunner.Benchmark(
      name,
      3,
      Iterations,
      () => assert(open(bytes).exists(_.document.paragraphs.size == ParagraphCount)),
      () => open(bytes)
    )

  private def document: RichTextDocument =
    RichTextDocument(
      (1 to ParagraphCount).toList.map { index =>
        RichTextParagraph(
          List(
            RichTextRun(s"Paragraph $index opens ", RichTextStyle.empty.withMark(InlineMark.Bold)),
            RichTextRun("with a longer run of ordinary body text that a manuscript would carry, "),
            RichTextRun("and closes in italics.", RichTextStyle.empty.withMark(InlineMark.Italic))
          )
        )
      }
    )

  private def docxFixture: Array[Byte] =
    withExtraParts(
      DocxDocumentCodec.writeBytes(document),
      List("word/styles.xml"       -> filler(200_000), "word/theme/theme1.xml" -> filler(60_000)),
      List("word/media/image1.png" -> noise(1_500_000))
    )

  private def odtFixture: Array[Byte] =
    withExtraParts(
      OdtDocumentCodec.writeBytes(document),
      List("styles.xml"          -> filler(200_000), "meta.xml" -> filler(4_000)),
      List("Pictures/image1.png" -> noise(1_500_000))
    )

  /** Rewrites a package with extra entries after its own: compressible XML parts, and incompressible media. */
  private def withExtraParts(
    original: Array[Byte],
    xmlParts: List[(String, Array[Byte])],
    mediaParts: List[(String, Array[Byte])]
  ): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output)
    try
      val input = java.util.zip.ZipInputStream(java.io.ByteArrayInputStream(original))
      try
        Iterator
          .continually(input.getNextEntry)
          .takeWhile(_ != null)
          .foreach { entry =>
            zip.putNextEntry(ZipEntry(entry.getName))
            zip.write(input.readAllBytes())
            zip.closeEntry()
          }
      finally input.close()
      (xmlParts ++ mediaParts).foreach { (name, content) =>
        zip.putNextEntry(ZipEntry(name))
        zip.write(content)
        zip.closeEntry()
      }
    finally zip.close()
    output.toByteArray

  private def filler(length: Int): Array[Byte] =
    LazyList
      .continually("<w:style w:styleId=\"Normal\"><w:name w:val=\"Normal\"/></w:style>\n")
      .flatMap(_.getBytes(StandardCharsets.UTF_8))
      .take(length)
      .toArray

  private def noise(length: Int): Array[Byte] =
    val bytes = Array.ofDim[Byte](length)
    Random(42).nextBytes(bytes)
    bytes

end RichDocumentOpenBenchmarks
