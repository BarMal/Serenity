package com.serenity.ui.openings

import com.serenity.config.MarkdownViewMode
import com.serenity.document.MarkdownHeadingIndex
import com.serenity.rope.{Balance, Rope}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MarkdownOpeningsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance                                     = Balance.default
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 1000)

  private val default = OpeningRules.default
  private val full    = default.copy(sceneBreak = SceneBreakRule.FullOpening)

  private def headings(lines: Seq[String]): MarkdownHeadingIndex =
    MarkdownHeadingIndex.build(Rope(lines.mkString("\n")))

  private def found(
    rules: OpeningRules,
    lines: String*
  ): Vector[Int] =
    foundIn(MarkdownViewMode.LivePreview, rules)(lines*).map(_.line)

  private def foundIn(mode: MarkdownViewMode, rules: OpeningRules)(lines: String*): Vector[Opening] =
    ChapterOpenings.openingLines(headings(lines), mode, rules, ChapterOpenings.noExplicitEpigraphs).openings

  "Markdown openings" should "be the first body paragraph after a level-one heading" in {
    found(default, "# One", "", "Opening.", "", "Next.") shouldBe Vector(2)
  }

  it should "need no blank line after the heading" in {
    found(default, "# One", "Opening.") shouldBe Vector(1)
  }

  it should "skip lower headings and blank lines" in {
    found(default, "# One", "", "## Part", "", "### Deeper", "", "Opening.") shouldBe Vector(6)
  }

  it should "be found after each heading" in {
    found(default, "# One", "A.", "", "# Two", "B.", "", "## Part", "C.") shouldBe Vector(1, 4)
  }

  it should "not exist when another level-one heading comes first" in {
    found(default, "# One", "", "# Two", "", "Opening.") shouldBe Vector(4)
    found(default, "# One", "# Two") shouldBe empty
  }

  it should "not follow a level-two heading alone" in {
    found(default, "## Part", "", "Text.") shouldBe empty
  }

  it should "follow a setext level-one heading, skipping its underline" in {
    found(default, "Title", "=====", "", "Opening.") shouldBe Vector(3)
    found(default, "Title", "=====", "Opening.") shouldBe Vector(2)
  }

  it should "not follow a setext level-two heading" in {
    found(default, "Title", "-----", "", "Text.") shouldBe empty
  }

  it should "ignore a heading inside a code fence" in {
    found(default, "```", "# not a title", "", "text", "```", "", "Prose.") shouldBe empty
    found(default, "# Real", "", "```", "# inner", "```", "") shouldBe empty
  }

  it should "ignore a heading inside a blockquote" in {
    found(default, "> # Quoted title", "", "Prose.") shouldBe empty
  }

  "Markdown openings" should "exist only in the preview modes" in {
    val text = Seq("# One", "", "Opening.")
    foundIn(MarkdownViewMode.Source, default)(text*) shouldBe empty
    foundIn(MarkdownViewMode.SplitPreview, default)(text*) shouldBe empty
    foundIn(MarkdownViewMode.LivePreview, default)(text*).map(_.line) shouldBe Vector(2)
    foundIn(MarkdownViewMode.Read, default)(text*).map(_.line) shouldBe Vector(2)
  }

  it should "never report a hand-made drop cap" in {
    foundIn(MarkdownViewMode.Read, default)("# One", "Opening.") shouldBe
      Vector(Opening(1, OpeningKind.Chapter, handDropCap = false))
  }

  "Lines that cannot open a chapter" should "end the search with no opening" in {
    val blockers = List(
      "- item",
      "* item",
      "+ item",
      "1. item",
      "2) item",
      "- [ ] task",
      "| a | b |",
      "```",
      "~~~",
      "    indented code",
      "\tindented code",
      "<div>html</div>",
      "![image](a.png)",
      "[ref]: http://example.com",
      "x",
      "1"
    )
    blockers.foreach { blocker =>
      withClue(s"'$blocker': ")(found(default, "# One", "", blocker, "", "Later.") shouldBe empty)
    }
  }

  it should "include a table header followed by its rule row" in {
    found(default, "# One", "Name | Age", "--- | ---", "Ann | 3") shouldBe empty
  }

  "The epigraph rule AutoDetect" should "skip a blockquote, however many lines it has" in {
    found(default, "# One", "", "> To the sea", "", "Opening.") shouldBe Vector(4)
    found(default, "# One", "> line one", "> line two", "Opening?") shouldBe empty
    found(default, "# One", "> line one", "lazy continuation", "", "Opening.") shouldBe Vector(4)
    found(default, "# One", "> a", "", "> b", "", "Opening.") shouldBe Vector(5)
  }

  "The epigraph rule Never" should "treat a blockquote as text that cannot open, and skip nothing" in {
    found(default.copy(epigraph = EpigraphRule.Never), "# One", "", "> To the sea", "", "Opening.") shouldBe empty
  }

  "The epigraph rule ExplicitOnly" should "skip only the lines the caller marks" in {
    val rules  = default.copy(epigraph = EpigraphRule.ExplicitOnly)
    val lines  = Seq("# One", "", "A marked line", "", "Opening.")
    val result = ChapterOpenings.openingLines(headings(lines), MarkdownViewMode.Read, rules, _ == 2)
    result.openings.map(_.line) shouldBe Vector(4)
    ChapterOpenings
      .openingLines(headings(lines), MarkdownViewMode.Read, rules, ChapterOpenings.noExplicitEpigraphs)
      .openings
      .map(_.line) shouldBe Vector(2)
  }

  it should "not skip an unmarked blockquote" in {
    val rules = default.copy(epigraph = EpigraphRule.ExplicitOnly)
    found(rules, "# One", "", "> quote", "", "Opening.") shouldBe empty
  }

  "Scene breaks" should "give no opening under NoOpening" in {
    found(default, "Before.", "", "***", "", "After.") shouldBe empty
  }

  it should "recognise every line of the scene break set between paragraphs" in
    List("***", "* * *", "---", "- - -", "___", "_ _ _", "#", "⁂", "• • •").foreach { mark =>
      withClue(s"'$mark': ")(found(full, "Before.", "", mark, "", "After.") shouldBe Vector(4))
    }

  it should "tag the opening with the rule that made it" in {
    val small = default.copy(sceneBreak = SceneBreakRule.SmallCapsOnly)
    foundIn(MarkdownViewMode.Read, small)("A.", "", "***", "", "B.").map(_.kind) shouldBe
      Vector(OpeningKind.SceneBreakSmallCaps)
    foundIn(MarkdownViewMode.Read, full)("A.", "", "***", "", "B.").map(_.kind) shouldBe
      Vector(OpeningKind.SceneBreakFull)
  }

  it should "read a --- under a paragraph as a setext underline, not a scene break" in {
    found(full, "Heading", "---", "", "After.") shouldBe empty
    found(full, "Heading", "---", "After.") shouldBe empty
  }

  it should "read a --- after a blank line as a thematic break" in {
    found(full, "Text.", "", "---", "After.") shouldBe Vector(3)
  }

  it should "not count a scene break inside a code fence" in {
    found(full, "```", "***", "", "text", "```") shouldBe empty
  }

  it should "end a chapter's search for its opening" in {
    found(default, "# One", "", "***", "", "After.") shouldBe empty
    found(full, "# One", "", "***", "", "After.") shouldBe Vector(4)
  }

  it should "treat a lone # as an empty heading, which is a scene break" in {
    found(full, "Text.", "", "#", "", "After.") shouldBe Vector(4)
    found(full, "Text.", "", "##", "", "After.") shouldBe empty
  }

  it should "not take the opening from a chapter whose title follows a scene break" in {
    found(full, "***", "# One", "Opening.") shouldBe Vector(2)
  }

  it should "give a list, table or one-character line after it no opening" in {
    found(full, "***", "", "- item", "", "After.") shouldBe empty
    found(full, "***", "x", "After.") shouldBe empty
  }

  private val vocabulary = Vector(
    "# One",
    "# Two",
    "## Part",
    "Title",
    "=====",
    "-----",
    "Body text.",
    "More body text.",
    "",
    "",
    "***",
    "---",
    "* * *",
    "#",
    "> quote",
    "lazy",
    "- item",
    "1. item",
    "```",
    "~~~",
    "x",
    "| a | b |",
    "--- | ---",
    "<div>",
    "    code"
  )

  private val genLines: Gen[Vector[String]] = Gen.listOfN(30, Gen.oneOf(vocabulary)).map(_.toVector)

  private val genRules: Gen[OpeningRules] =
    for
      epigraph   <- Gen.oneOf(EpigraphRule.values.toSeq)
      sceneBreak <- Gen.oneOf(SceneBreakRule.values.toSeq)
    yield OpeningRules(epigraph, sceneBreak)

  private def marks(lines: Vector[String]): Int => Boolean = line => lines.lift(line).contains("lazy")

  final private case class EditCase(
      rules: OpeningRules,
      original: Vector[String],
      replacement: List[String],
      at: Int,
      removed: Int,
      mode: MarkdownViewMode
  )

  private val genEditCase: Gen[EditCase] =
    for
      rules       <- genRules
      original    <- genLines
      replacement <- Gen.listOfN(4, Gen.oneOf(vocabulary))
      at          <- Gen.choose(0, 30)
      removed     <- Gen.choose(0, 4)
      mode        <- Gen.oneOf(MarkdownViewMode.LivePreview, MarkdownViewMode.Read)
    yield EditCase(rules, original, replacement, at, removed, mode)

  "Markdown opening detection" should "agree with a full rescan after any edit" in
    forAll(genEditCase, Gen.choose(0, 4)) { (found, inserted) =>
      val EditCase(rules, original, replacement, at, removed, mode) = found
      val start                                                     = at.min(original.size)
      val removing                                                  = original.slice(start, start + removed).length
      val added                                                     = replacement.take(inserted).toVector
      val edited                                                    = original.patch(start, added, removing)
      val before                                                    = headings(original)
      val after                                                     = before.updatedTo(Rope(edited.mkString("\n")))
      val previous = ChapterOpenings.openingLines(before, mode, rules, marks(original))
      val edit     = LineEdit.between(before.content, after.content)
      val updated  = ChapterOpenings.updatedOpeningLines(previous, after, edit, mode, rules, marks(edited))
      withClue(s"$original -> $edited, $edit: ")(
        updated shouldBe ChapterOpenings.openingLines(after, mode, rules, marks(edited))
      )
    }

  it should "keep agreeing across a chain of edits" in
    forAll(genRules, genLines, Gen.listOfN(12, Gen.zip(Gen.choose(0, 30), Gen.oneOf(vocabulary)))) {
      (rules, original, edits) =>
        val mode  = MarkdownViewMode.Read
        val first = headings(original)
        val start =
          (first, ChapterOpenings.openingLines(first, mode, rules, ChapterOpenings.noExplicitEpigraphs), original)
        val (index, openings, lines) = edits.foldLeft(start) {
          case ((index, openings, lines), (at, text)) =>
            val position = at.min(lines.size - 1).max(0)
            val next     = lines.updated(position, text)
            val updated  = index.updatedTo(Rope(next.mkString("\n")))
            val edit     = LineEdit.between(index.content, updated.content)
            (
              updated,
              ChapterOpenings.updatedOpeningLines(
                openings,
                updated,
                edit,
                mode,
                rules,
                ChapterOpenings.noExplicitEpigraphs
              ),
              next
            )
        }
        val classes = lines.indices.map(MarkdownOpenings.classify(index, rules, ChapterOpenings.noExplicitEpigraphs))
        withClue(s"$original, $edits, $lines, $rules, $classes, ${index.headings}: ")(
          openings shouldBe ChapterOpenings.openingLines(index, mode, rules, ChapterOpenings.noExplicitEpigraphs)
        )
        lines.size shouldBe original.size
    }

  "LineEdit.between" should "name the lines two ropes differ in" in {
    val before = Rope("a\nb\nc\nd")
    LineEdit.between(before, Rope("a\nB\nc\nd")) shouldBe LineEdit(1, 1, 1)
    LineEdit.between(before, Rope("a\nb\nx\nc\nd")) shouldBe LineEdit(2, 1, 2)
    LineEdit.between(before, before) shouldBe LineEdit(0, 0, 0)
  }

  it should "take the lines from a change when given one" in {
    val before = Rope("a\nb\nc")
    val after  = Rope("a\nbb\nc")
    val change = com.serenity.rope.ChangeSet.single(before.weight, 3, 3, "b")
    LineEdit.between(before, after, Some(change)) shouldBe LineEdit(1, 1, 1)
  }
