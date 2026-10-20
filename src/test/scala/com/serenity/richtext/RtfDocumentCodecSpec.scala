package com.serenity.richtext

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RtfDocumentCodecSpec extends AnyFlatSpec with Matchers:

  "RtfDocumentCodec" should "read inline marks and paragraph alignment from RTF" in {
    val rtf =
      """{\rtf1\ansi\pard\qc plain \b bold\b0  \i italic\i0  \ul under\ul0\par}"""

    val document  = decode(rtf.getBytes(StandardCharsets.UTF_8))
    val paragraph = singleParagraph(document)

    paragraph.alignment shouldBe ParagraphAlignment.Center
    paragraph.plainText should include("plain bold italic under")
    marksForText(paragraph, "bold") should contain(InlineMark.Bold)
    marksForText(paragraph, "italic") should contain(InlineMark.Italic)
    marksForText(paragraph, "under") should contain(InlineMark.Underline)
  }

  it should "write native rich text documents as readable RTF" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("Hello ", RichTextStyle.empty),
            RichTextRun("world", RichTextStyle(marks = Set(InlineMark.Bold, InlineMark.Underline)))
          ),
          alignment = ParagraphAlignment.Right
        )
      )
    )

    val decoded   = decode(RtfDocumentCodec.writeBytes(source))
    val paragraph = singleParagraph(decoded)

    paragraph.alignment shouldBe ParagraphAlignment.Right
    paragraph.plainText shouldBe "Hello world"
    marksForText(paragraph, "world") should contain allOf (InlineMark.Bold, InlineMark.Underline)
  }

  it should "preserve explicit font size and colour metadata" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          List(
            RichTextRun(
              "styled",
              RichTextStyle(fontSize = Some(18.0f), color = Some("#336699"))
            )
          )
        )
      )
    )

    val decodedStyle = singleParagraph(decode(RtfDocumentCodec.writeBytes(source))).runs
      .find(_.text.contains("styled"))
      .map(_.style)

    decodedStyle.flatMap(_.fontSize).map(_.round) shouldBe Some(18)
    decodedStyle.flatMap(_.color) shouldBe Some("#336699")
  }

  it should "read native RTF tab and line controls as inline structural text" in {
    val rtf =
      """{\rtf1\ansi\pard alpha\tab beta\line gamma\par}"""

    val decoded = decode(rtf.getBytes(StandardCharsets.UTF_8))

    singleParagraph(decoded).runs shouldBe
      List(RichTextRun("alpha\tbeta"), RichTextRun.softBreak(), RichTextRun("gamma"))
    decoded.exportText shouldBe "alpha\tbeta\ngamma"
  }

  it should "write tabs and line breaks as native RTF controls" in {
    val source = RichTextDocument(
      List(RichTextParagraph(List(RichTextRun("alpha\tbeta"), RichTextRun.softBreak(), RichTextRun("gamma"))))
    )

    val bytes   = RtfDocumentCodec.writeBytes(source)
    val rtfText = String(bytes, StandardCharsets.UTF_8)
    val decoded = decode(bytes)

    rtfText should include("\\tab")
    rtfText should include("\\line")
    singleParagraph(decoded).runs shouldBe
      List(RichTextRun("alpha\tbeta"), RichTextRun.softBreak(), RichTextRun("gamma"))
    decoded.exportText shouldBe "alpha\tbeta\ngamma"
  }

  it should "round-trip a heading paragraph as a heading, not as bold enlarged text" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(List(RichTextRun("Chapter One")), role = ParagraphRole.Heading(1)),
        RichTextParagraph(List(RichTextRun("Section")), role = ParagraphRole.Heading(3)),
        RichTextParagraph(List(RichTextRun("Body copy")))
      )
    )

    val bytes   = RtfDocumentCodec.writeBytes(source)
    val decoded = decode(bytes)

    decoded.paragraphs.map(_.role) shouldBe List(
      ParagraphRole.Heading(1),
      ParagraphRole.Heading(3),
      ParagraphRole.Body
    )
    decoded.paragraphs.flatMap(_.runs).map(_.style) shouldBe List.fill(3)(RichTextStyle.empty)
    val rtf = String(bytes, StandardCharsets.ISO_8859_1)
    rtf should include("heading 1")
    rtf should include("\\outlinelevel0")
    rtf should include("\\outlinelevel2")
  }

  it should "round-trip a drop cap paragraph with its line span" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(List(RichTextRun("Chapter One")), role = ParagraphRole.DropCap(4)),
        RichTextParagraph(List(RichTextRun("Body copy")))
      )
    )

    val decoded = decode(RtfDocumentCodec.writeBytes(source))

    decoded.paragraphs.map(_.role) shouldBe List(ParagraphRole.DropCap(4), ParagraphRole.Body)
    decoded.paragraphs.head.runs shouldBe List(RichTextRun("Chapter One"))
  }

  it should "read headings from \\outlinelevel, from heading-named styles and from the stylesheet's outline level" in {
    val document = imported(
      """{\rtf1\ansi{\stylesheet{\s0 Normal;}{\s1\outlinelevel0 Title Style;}{\s2 Heading 2;}{\s3 Plain;}}
        |\pard\outlinelevel2 direct\par
        |\pard\s2 named\par
        |\pard\s1 inherited\par
        |\pard\s3 body\par}""".stripMargin
    ).document

    document.paragraphs.map(_.role) shouldBe List(
      ParagraphRole.Heading(3),
      ParagraphRole.Heading(2),
      ParagraphRole.Heading(1),
      ParagraphRole.Body
    )
  }

  it should "apply paragraph and character styles from the stylesheet beneath direct formatting" in {
    val document = imported(
      """{\rtf1\ansi{\fonttbl{\f3 Georgia;}}{\stylesheet{\s0 Normal;}{\s5\i\f3\fs30 Quote;}{\*\cs9\b Strong;}}
        |\pard\s5 quoted \cs9 strong\b0  weak\par}""".stripMargin
    ).document

    val runs = document.paragraphs.flatMap(_.runs)
    runs.map(_.text) shouldBe List("quoted ", "strong", " weak")
    runs.map(_.style.marks) shouldBe List(
      Set(InlineMark.Italic),
      Set(InlineMark.Italic, InlineMark.Bold),
      Set(InlineMark.Italic)
    )
    runs.map(_.style.fontFamily).distinct shouldBe List(Some("Georgia"))
    runs.map(_.style.fontSize).distinct shouldBe List(Some(15.0f))
  }

  it should "decode \\uN escapes honouring \\ucN skip counts" in {
    textOf("""{\rtf1\ansi\uc1\pard Euro \u8364? sign\par}""") shouldBe "Euro \u20ac sign"
    textOf("""{\rtf1\ansi\uc2\pard A\u8364 ??B\par}""") shouldBe "A\u20acB"
    textOf("""{\rtf1\ansi\uc0\pard A\u8364 B\par}""") shouldBe "A\u20acB"
    textOf("""{\rtf1\ansi\uc1\pard A\u8364\'80B\par}""") shouldBe "A\u20acB"
    textOf("""{\rtf1\ansi\uc1\pard \u-3913 ?\par}""") shouldBe "\uf0b7"
    textOf("""{\rtf1\ansi\uc1\pard \u-10179?\u-8704?\par}""") shouldBe "\ud83d\ude00"
  }

  it should "scope \\ucN to its group" in {
    textOf("""{\rtf1\ansi\uc1\pard {\uc3 A\u8364 xyzB}C\u8364?D\par}""") shouldBe "A\u20acBC\u20acD"
  }

  it should "write non-ASCII text as \\uN escapes with an ASCII fallback and read it back" in {
    val text   = "caf\u00e9 \u20ac \ud83d\ude00 \\ { }"
    val source = RichTextDocument.oneParagraph(text)

    val bytes = RtfDocumentCodec.writeBytes(source)

    bytes.forall(byte => byte >= 0 && byte < 0x80) shouldBe true
    val rtf = String(bytes, StandardCharsets.US_ASCII)
    rtf should include("\\u233e")
    rtf should include("\\u8364?")
    decode(bytes).plainText shouldBe text
  }

  it should "decode \\'hh escapes in the declared ANSI code page" in {
    textOf("""{\rtf1\ansi\ansicpg1252\pard caf\'e9\par}""") shouldBe "caf\u00e9"
    textOf("""{\rtf1\ansi\ansicpg1251\pard \'cf\'f0\par}""") shouldBe "\u041f\u0440"
    textOf("""{\rtf1\ansi\ansicpg932\pard \'93\'fa\par}""") shouldBe "\u65e5"
    textOf("""{\rtf1\ansi\ansicpg1250\pard \'e8\par}""") shouldBe "\u010d"
  }

  it should "decode \\'hh escapes using the current font's charset" in {
    textOf(
      """{\rtf1\ansi\ansicpg1252{\fonttbl{\f0\fcharset0 A;}{\f1\fcharset204 B;}}\pard \'e9{\f1 \'e9}\par}"""
    ) shouldBe
      "\u00e9\u0439"
  }

  it should "keep \\line as an inline line break and decode tab and special-character controls" in {
    textOf("""{\rtf1\ansi\pard a\line  b\tab c\~d\emdash e\lquote f\rquote \\\{\}\par}""") shouldBe
      "a\n b\tc\u00a0d\u2014e\u2018f\u2019\\{}"
  }

  it should "write line breaks without any private-use marker bytes" in {
    val bytes = RtfDocumentCodec.writeBytes(RichTextDocument.oneParagraph("alpha\ngamma"))
    val rtf   = String(bytes, StandardCharsets.ISO_8859_1)

    rtf should include("\\line")
    rtf should not include "57344"
    decode(bytes).exportText shouldBe "alpha\ngamma"
  }

  it should "report tables as unsupported while keeping their cell text" in {
    val result = imported(
      """{\rtf1\ansi\trowd\trgaph108\cellx2000\cellx4000
        |\pard\intbl first\cell second\cell\row
        |\pard after\par}""".stripMargin
    )

    result.fidelity.unsupportedElements should contain("table")
    result.fidelity.count(DocumentFeature.Tables, Treatment.Dropped) shouldBe 1
    result.fidelity.dropSummary shouldBe "1 table"
    result.fidelity.isLossless shouldBe false
    result.document.plainText should include("first")
    result.document.plainText should include("second")
    result.document.paragraphs.lastOption.map(_.plainText) shouldBe Some("after")
  }

  it should "report pictures as unsupported without leaking their data into the text" in {
    val result = imported(
      """{\rtf1\ansi\pard before {\pict\pngblip\picw10\pich10\picwgoal200\pichgoal200 89504e470d0a1a0a} after\par}"""
    )

    result.fidelity.unsupportedElements should contain("picture")
    result.fidelity.count(DocumentFeature.Images, Treatment.Dropped) shouldBe 1
    result.fidelity.dropSummary shouldBe "1 image"
    result.fidelity.isLossless shouldBe false
    result.document.plainText shouldBe "before  after"
  }

  it should "report other constructs it cannot represent" in {
    val fidelity = imported(
      """{\rtf1\ansi\pard \strike gone\strike0  {\super up}{\field{\*\fldinst HYPERLINK "http://example.com"}{\fldrslt link}}{\footnote note}\par}"""
    ).fidelity

    fidelity.unsupportedElements should contain allOf ("strikethrough", "superscript", "field", "footnote")
    fidelity.count(DocumentFeature.Fields, Treatment.Dropped) shouldBe 1
    fidelity.count(DocumentFeature.Notes, Treatment.Dropped) shouldBe 1
    fidelity.count(DocumentFeature.Other("strikethrough"), Treatment.Dropped) shouldBe 1
  }

  it should "skip unknown destinations as opaque groups and report them by name" in {
    val result = imported("""{\rtf1\ansi\pard keep{\*\madeup \b hidden {nested} text}\par}""")

    result.document.plainText shouldBe "keep"
    result.fidelity.unsupportedElements should contain("destination:madeup")
  }

  it should "not report benign metadata destinations" in {
    imported(
      """{\rtf1\ansi{\*\generator Word}{\info{\title T}}{\*\listtable}{\*\rsidtbl \rsid1}\pard text\par}"""
    ).fidelity.isLossless shouldBe true
  }

  it should "import a Word-style document with styles, fonts, colours, code pages and unknown destinations" in {
    val result =
      RtfDocumentCodec.readBytesWithFidelity(fixture("word-sample.rtf")).fold(e => fail(e.getMessage), identity)
    val document = result.document

    document.paragraphs.map(_.exportText) shouldBe List(
      "Quarterly Report",
      "Caf\u00e9 na\u00efve bold and italic and under\nnext line with \u20ac",
      "Centered red",
      "Subheading",
      "\u041f\u0440\u0438\u0432\u0435\u0442",
      "Plain tail"
    )
    document.paragraphs.map(_.role) shouldBe
      List(ParagraphRole.Heading(1), ParagraphRole.Body, ParagraphRole.Body, ParagraphRole.Heading(2)) ++
      List(ParagraphRole.Body, ParagraphRole.Body)
    document.paragraphs.lift(2).map(_.alignment) shouldBe Some(ParagraphAlignment.Center)
    document.paragraphs.lift(2).flatMap(_.runs.headOption).flatMap(_.style.color) shouldBe Some("#ff0000")
    document.paragraphs.lift(1).map(p => marksForText(p, "bold")) shouldBe Some(Set(InlineMark.Bold))
    document.paragraphs.lift(1).map(p => marksForText(p, "italic")) shouldBe Some(Set(InlineMark.Italic))
    document.paragraphs.lift(1).map(p => marksForText(p, "under")) shouldBe Some(Set(InlineMark.Underline))
    document.paragraphs.lift(0).map(_.runs.flatMap(_.style.marks).toSet) shouldBe Some(Set.empty[InlineMark])
    result.fidelity.unsupportedElements shouldBe Set("destination:xyzzy")
  }

  it should "return an error rather than throwing for malformed input" in {
    val malformed = List(
      "",
      "plain text, not RTF",
      """{\rtf1\ansi {\b bold}""",
      """{\rtf1\ansi caf\'""",
      """{\rtf1\ansi caf\""",
      """{\rtf1 a}}""",
      """{\rtf1 {\pict\bin100 abc}}""",
      "{" * 50000
    )

    malformed.foreach(rtf => RtfDocumentCodec.readBytes(rtf.getBytes(StandardCharsets.ISO_8859_1)).isLeft shouldBe true)
  }

  it should "never throw on any truncation of a real document" in {
    val full = fixture("word-sample.rtf")

    (0 until full.length).foreach { length =>
      noException should be thrownBy RtfDocumentCodec.readBytes(full.take(length))
    }
  }

  it should "report an error from IO when the file is not RTF" in {
    val path = TestTemp.file("serenity-not-rtf", ".rtf")
    try
      Files.writeString(path, "just words")
      RtfDocumentCodec.read(path).attempt.unsafeRunSync().isLeft shouldBe true
    finally Files.deleteIfExists(path)
  }

  it should "write a self-contained document that re-reads without any fidelity warnings" in {
    val source = RichTextDocument(
      List(
        RichTextParagraph(List(RichTextRun("Title")), role = ParagraphRole.Heading(2)),
        RichTextParagraph(
          List(
            RichTextRun("plain "),
            RichTextRun("red", RichTextStyle(Set(InlineMark.Bold), Some("Georgia"), Some(13.5f), Some("#ff0000")))
          ),
          ParagraphAlignment.Justify
        )
      )
    )

    val result = RtfDocumentCodec
      .readBytesWithFidelity(RtfDocumentCodec.writeBytes(source))
      .fold(e => fail(e.getMessage), identity)

    result.fidelity.isLossless shouldBe true
    result.document.paragraphs shouldBe source.paragraphs
  }

  it should "preserve empty paragraphs through an RTF round trip" in {
    val source = RichTextDocument.fromPlainText("First\n\nThird")

    decode(RtfDocumentCodec.writeBytes(source)).plainText shouldBe "First\n\nThird"
  }

  it should "read and write RTF files through IO" in {
    val path   = TestTemp.file("serenity-rich-text", ".rtf")
    val source = RichTextDocument.oneParagraph("Saved text")

    try
      RtfDocumentCodec.write(source, path).unsafeRunSync()

      val loaded = RtfDocumentCodec.read(path).unsafeRunSync()

      loaded.plainText shouldBe "Saved text"
    finally Files.deleteIfExists(path)
  }

  private def decode(bytes: Array[Byte]): RichTextDocument =
    RtfDocumentCodec.readBytes(bytes).fold(error => fail(error.getMessage), identity)

  private def imported(rtf: String): RichTextImport =
    RtfDocumentCodec
      .readBytesWithFidelity(rtf.getBytes(StandardCharsets.ISO_8859_1))
      .fold(error => fail(error.getMessage), identity)

  private def textOf(rtf: String): String = imported(rtf).document.exportText

  private def fixture(name: String): Array[Byte] =
    Files.readAllBytes(Paths.get(getClass.getResource(s"/richtext/$name").toURI))

  private def marksForText(paragraph: RichTextParagraph, text: String): Set[InlineMark] =
    paragraph.runs
      .find(_.text.contains(text))
      .map(_.style.marks)
      .getOrElse(Set.empty)

  private def singleParagraph(document: RichTextDocument): RichTextParagraph =
    document.paragraphs match
      case paragraph :: Nil => paragraph
      case other            => fail(s"Expected one paragraph, got ${other.size}")
