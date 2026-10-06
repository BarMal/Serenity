package com.serenity.manuscript

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManuscriptConfSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val defaults = CompileSpec.forTitle("draft")

  "ManuscriptConf" should "keep the defaults for an empty file" in {
    ManuscriptConf.decode("", defaults).value shouldBe defaults
  }

  it should "decode the e-book language and identifier, trimmed, and ignore blank ones" in {
    val configured =
      ManuscriptConf.decode("language = \" fr-CA \"\nidentifier = \" urn:isbn:9780000000002 \"", defaults).value
    val blank = ManuscriptConf.decode("language = \" \"\nidentifier = \"\"", defaults).value

    (configured.language, configured.identifier) shouldBe ("fr-CA", Some("urn:isbn:9780000000002"))
    (blank.language, blank.identifier) shouldBe ("en", None)
  }

  it should "decode every key" in {
    val conf =
      """title = "The Long Night"
        |short-title = "NIGHT"
        |author = "Jane Q. van Writer"
        |surname = "van Writer"
        |byline = "J. Q. Writer"
        |contact = ["1 High Street", "jane@example.com"]
        |sources = [{ path = "01.md" }, { path = "notes.md", exclude = true }]
        |format = "classic"
        |paper = "a4"
        |chapter-heading = "Chapter <$n>\n<$t>"
        |part-heading = "Part <$R>"
        |part-level = 1
        |chapter-level = 2
        |scene-break-patterns = ["~"]
        |transforms = ["smart-punctuation", "italics-as-underline"]
        |replacements = [{ find = "Bob", replace = "Robert" }, { find = "a+", replace = "b", regex = true }]
        |title-page = false
        |word-count = "short-fiction"
        |dedication = "For M."
        |end-marker = ""
        |""".stripMargin

    ManuscriptConf.decode(conf, defaults).value shouldBe CompileSpec(
      title = "The Long Night",
      shortTitle = Some("NIGHT"),
      author = AuthorName("Jane Q. van Writer", "van Writer"),
      byline = Some("J. Q. Writer"),
      contact = List("1 High Street", "jane@example.com"),
      sources = List(SourceEntry("01.md", include = true), SourceEntry("notes.md", include = false)),
      rules = SectionRules(Some(1), 2, Set("~")),
      chapterHeading = HeadingTemplate("Chapter <$n>\n<$t>"),
      partHeading = HeadingTemplate("Part <$R>"),
      transforms = List(TextTransform.SmartPunctuation, TextTransform.ItalicsAsUnderline),
      replacements = List(Replacement("Bob", "Robert", regex = false), Replacement("a+", "b", regex = true)),
      titlePage = false,
      wordCountRounding = WordCountRounding.ShortFiction,
      dedication = Some("For M."),
      endMarker = None,
      format = ManuscriptFormat.Classic.copy(paper = PaperSize.A4)
    )
  }

  it should "take a format's own transforms when none are listed" in {
    ManuscriptConf.decode("format = classic", defaults).value.transforms shouldBe
      ManuscriptFormat.Classic.defaultTransforms
  }

  it should "report unknown names and invalid levels" in {
    ManuscriptConf.decode("format = fancy", defaults).left.value shouldBe
      CompileError.InvalidConfiguration("unknown format 'fancy'")
    ManuscriptConf.decode("transforms = [shout]", defaults).left.value shouldBe
      CompileError.InvalidConfiguration("unknown transform 'shout'")
    ManuscriptConf
      .decode("part-level = 2\nchapter-level = 1", defaults)
      .left
      .value shouldBe a[CompileError.InvalidConfiguration]
  }

  it should "report malformed HOCON and wrongly typed values" in {
    ManuscriptConf.decode("title = [", defaults).left.value shouldBe a[CompileError.InvalidConfiguration]
    ManuscriptConf.decode("title-page = maybe", defaults).left.value shouldBe a[CompileError.InvalidConfiguration]
  }
