package com.serenity.manuscript

import com.serenity.richtext.{InlineMark, RichTextRun, RichTextStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/** Raw HTML in a manuscript is the writer's markup, not an instruction to a browser: it maps to the marks the model
  * has, and everything else keeps its text. Tags themselves never survive into a [[Block]].
  */
class MarkdownManuscriptHtmlSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private def blocks(markdown: String): Vector[Block] =
    MarkdownManuscript.sections(markdown, SectionRules.default) match
      case Vector(Section.Chapter(None, found)) => found
      case other                                => fail(s"expected one headingless chapter, got $other")

  private def marked(text: String, marks: InlineMark*): RichTextRun =
    RichTextRun(text, RichTextStyle(marks = marks.toSet))

  private def body(runs: RichTextRun*): Block =
    Block.Paragraph(runs.toList, ParagraphKind.Body)

  private def plain(markdown: String): Vector[String] =
    blocks(markdown).collect { case Block.Paragraph(runs, _) => ManuscriptText.plainText(runs) }

  private val markTags = Table(
    ("open", "close", "mark"),
    ("<b>", "</b>", InlineMark.Bold),
    ("<strong>", "</strong>", InlineMark.Bold),
    ("<i>", "</i>", InlineMark.Italic),
    ("<em>", "</em>", InlineMark.Italic),
    ("<u>", "</u>", InlineMark.Underline),
    ("<B>", "</B>", InlineMark.Bold),
    ("""<b class="loud">""", "</b>", InlineMark.Bold),
    ("<em  >", "</em >", InlineMark.Italic)
  )

  "MarkdownManuscript" should "turn inline mark tags into marks, whatever their spelling" in
    forAll(markTags) { (open, close, mark) =>
      blocks(s"a ${open}word$close b") shouldBe Vector(body(RichTextRun("a "), marked("word", mark), RichTextRun(" b")))
    }

  it should "combine nested mark tags and tags nested in Markdown emphasis" in {
    blocks("<b><i>both</i></b> and *<b>mixed</b>*") shouldBe Vector(
      body(
        marked("both", InlineMark.Bold, InlineMark.Italic),
        RichTextRun(" and "),
        marked("mixed", InlineMark.Bold, InlineMark.Italic)
      )
    )
  }

  it should "end an unclosed mark tag with its paragraph" in {
    blocks("<b>never closed\n\nNext.") shouldBe Vector(
      body(marked("never closed", InlineMark.Bold)),
      body(RichTextRun("Next."))
    )
  }

  it should "read every spelling of an inline line break as a line break" in
    forAll(Table("tag", "<br>", "<br/>", "<br />", "<BR>"))(tag => plain(s"one${tag}two") shouldBe Vector("one\ntwo"))

  it should "keep the text of superscript and subscript, which the model has no mark for" in {
    plain("E = mc<sup>2</sup> and H<sub>2</sub>O") shouldBe Vector("E = mc2 and H2O")
    blocks("x<sup>2</sup>") shouldBe Vector(body(RichTextRun("x2")))
  }

  it should "strip other inline tags and keep their text" in
    forAll(
      Table(
        ("markdown", "text"),
        ("""a <span class="x">kept</span> b""", "a kept b"),
        ("""see <a href="https://example.com">the link</a>""", "see the link"),
        ("""<font color="red">red</font> <mark>marked</mark> <small>small</small>""", "red marked small"),
        ("""before <img src="x.png" alt="pic"> after""", "before  after"),
        ("a<!-- an editor's note -->b", "ab")
      )
    )((markdown, text) => plain(markdown) shouldBe Vector(text))

  it should "never leave an angle-bracket tag in the text of a paragraph" in
    forAll(
      Table(
        "markdown",
        "<b>x</b> <i>y</i> <u>z</u> <br> <sup>1</sup> <span>s</span>",
        "<div>block <b>b</b></div>",
        "<p>one</p>\n<p>two <em>e</em></p>",
        "<iframe src=\"https://example.com\"></iframe>text"
      )
    )(markdown => plain(markdown).foreach(text => text should not include regex("</?[A-Za-z]")))

  it should "keep a backslash-escaped tag as literal text" in {
    blocks("""Salt \<b>not bold\</b>.""") shouldBe Vector(body(RichTextRun("Salt <b>not bold</b>.")))
  }

  it should "keep the text of a heading that contains inline tags" in {
    MarkdownManuscript.sections("# Hello <b>World</b>\n\nText.", SectionRules.default) shouldBe Vector(
      Section.Chapter(Some(SectionHeading.of("Hello World")), Vector(body(RichTextRun("Text."))))
    )
  }

  it should "keep the text of a block of HTML as a paragraph, with its marks" in {
    blocks("<div>Hello <b>world</b></div>") shouldBe Vector(
      body(RichTextRun("Hello "), marked("world", InlineMark.Bold))
    )
  }

  it should "join the lines of a block of HTML and decode its entities" in {
    plain("<div>\nTom &amp; Jerry\nare &lt;3 &#33; &#x21;\n</div>") shouldBe Vector("Tom & Jerry are <3 ! !")
  }

  it should "give each HTML paragraph, heading and list item its own paragraph" in {
    plain("<h2>Title</h2>\n<p>One</p>\n<p>Two <em>twice</em></p>\n<ul><li>a</li><li>b</li></ul>") shouldBe
      Vector("Title", "One", "Two twice", "a", "b")
  }

  it should "keep a line break inside a block of HTML" in {
    plain("<p>one<br>two</p>") shouldBe Vector("one\ntwo")
  }

  it should "keep the quote kind of a block of HTML inside a block quote" in {
    blocks("> <div>Quoted.</div>") shouldBe Vector(
      Block.Paragraph(List(RichTextRun("Quoted.")), ParagraphKind.BlockQuote)
    )
  }

  it should "read a horizontal rule as a scene break" in {
    blocks("Before.\n\n<hr>\n\nAfter.") shouldBe Vector(
      body(RichTextRun("Before.")),
      Block.SceneBreak,
      body(RichTextRun("After."))
    )
  }

  it should "drop comments, scripts and style sheets, which are not prose, but nothing else" in {
    blocks(
      "Before.\n\n<!-- a note\nto self -->\n\n<style>p { color: red }</style>\n\n<script>go()</script>\n\nAfter."
    ) shouldBe
      Vector(body(RichTextRun("Before.")), body(RichTextRun("After.")))
  }

  it should "count the words of kept HTML text" in {
    ManuscriptText.wordCount(
      MarkdownManuscript.sections("<div>one <b>two</b> three</div>", SectionRules.default)
    ) shouldBe 3
  }
