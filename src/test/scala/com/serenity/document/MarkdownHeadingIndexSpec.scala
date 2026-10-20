package com.serenity.document

import scala.util.Random

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.state.models.{Buffer, BufferId, HeadingIdentity}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownHeadingIndexSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def novel(chapters: Int): String =
    (1 to chapters)
      .map(n => s"# Chapter $n\n\nSome prose for chapter $n.\n\nMore prose, still chapter $n.")
      .mkString("\n\n")

  private def insertedAt(rope: Rope, offset: Int, text: String): Rope =
    rope.insert(offset, text).getOrElse(rope)

  private def deleted(rope: Rope, from: Int, until: Int): Rope =
    rope.delete(from, until).getOrElse(rope)

  private def agreesWithFullParse(index: MarkdownHeadingIndex, content: Rope): Unit =
    val full = MarkdownHeadingIndex.build(content)
    index.headings shouldBe full.headings
    index.fences shouldBe full.fences

  "an incremental update" should "parse only the blocks around a keystroke in a novel-length text" in {
    val before = Rope(novel(chapters = 1_000))
    val index  = MarkdownHeadingIndex.build(before)
    index.parsedLines should be > 5_000

    val offset = before.collect().indexOf("Some prose for chapter 500") + 4
    val after  = insertedAt(before, offset, "x")
    val next   = index.updatedTo(after)

    next.parsedLines should be < 20
    agreesWithFullParse(next, after)
  }

  it should "parse only the blocks around a typed heading and renumber every heading after it" in {
    val before = Rope(novel(chapters = 300))
    val index  = MarkdownHeadingIndex.build(before)
    val offset = before.collect().indexOf("Some prose for chapter 10.")
    val after  = insertedAt(before, offset, "\n\n## New scene\n\n")
    val next   = index.updatedTo(after)

    next.parsedLines should be < 20
    next.headings.map(_.name).slice(9, 13) shouldBe List("Chapter 10", "New scene", "Chapter 11", "Chapter 12")
    agreesWithFullParse(next, after)
  }

  it should "reparse nothing when an edit leaves the text as it was" in {
    val before = Rope(novel(chapters = 50))
    val index  = MarkdownHeadingIndex.build(before)
    val same   = insertedAt(before, 3, "x").delete(3, 4).getOrElse(before)
    val next   = index.updatedTo(same)

    next.parsedLines shouldBe 0
    next.headings shouldBe index.headings
  }

  it should "follow a fence opened by typing, which hides the headings it swallows" in {
    val text   = "# One\n\nprose\n\n# Two\n\nprose\n\n```\ncode\n```\n\n# Three\n"
    val before = Rope(text)
    val after  = insertedAt(before, text.indexOf("# Two"), "```\n")
    val next   = MarkdownHeadingIndex.build(before).updatedTo(after)

    agreesWithFullParse(next, after)
    next.headings.map(_.name) shouldBe List("One")
  }

  it should "not treat a heading-looking line inside a fenced block as a heading" in {
    val text   = "# One\n\n```\n# not a heading\n\ntext\n```\n\n# Two\n"
    val before = Rope(text)
    val index  = MarkdownHeadingIndex.build(before)
    index.headings.map(_.name) shouldBe List("One", "Two")

    val after = insertedAt(before, text.indexOf("# Two"), "more ")
    agreesWithFullParse(index.updatedTo(after), after)
  }

  it should "agree with a full parse when an HTML comment hides headings across blank lines" in {
    val text   = "# One\n\n<!--\n\n# hidden\n\n-->\n\n# Two\n"
    val before = Rope(text)
    val after  = insertedAt(before, text.indexOf("# hidden") + 2, "very ")
    agreesWithFullParse(MarkdownHeadingIndex.build(before).updatedTo(after), after)
  }

  it should "agree with a full parse when a link reference definition changes a heading's title" in {
    val text   = "# See [docs][d]\n\nprose\n\n[d]: https://example.com\n"
    val before = Rope(text)
    val after  = deleted(before, text.indexOf("[d]:"), text.length)
    agreesWithFullParse(MarkdownHeadingIndex.build(before).updatedTo(after), after)
  }

  it should "not keep a heading that only stood after the edit because it continued a block the edit removed" in {
    val text   = "Title\n====\n\n> quote\nOther\n---\n\nend\n"
    val before = Rope(text)
    val after  = deleted(before, text.indexOf("====") + 5, text.indexOf("Other"))
    val next   = MarkdownHeadingIndex.build(before).updatedTo(after)

    agreesWithFullParse(next, after)
  }

  it should "keep chapter-note identities equal to those computed from the headings after an edit" in {
    val before = Rope("# Chapter 1: Storm\n\ntext\n\n# Chapter 2: Storm\n\n# Calm\n")
    val after  = insertedAt(before, before.collect().indexOf("text"), "more ")
    val next   = MarkdownHeadingIndex.build(before).updatedTo(after)

    next.identified shouldBe HeadingIdentity.forHeadings(next.headings.toList)
    next.identified.map(_._1) shouldBe List(
      HeadingIdentity("storm", 0),
      HeadingIdentity("storm", 1),
      HeadingIdentity("calm", 0)
    )
  }

  private val safePieces = Vector(
    "# Chapter 1\n",
    "## Scene\n",
    "\n",
    "\n\n",
    "prose line\n",
    "Setext title\n====\n",
    "Other\n---\n",
    "```\n",
    "~~~\n# fenced\n~~~\n",
    "- item\n",
    "  - nested\n",
    "> quote\n",
    "> # quoted\n",
    "    indented code\n",
    "| a | b |\n| - | - |\n",
    "#### Deep ####\n",
    "x"
  )

  private val hazardousPieces = safePieces ++ Vector("<!-- note\n", "-->\n", "[ref]: https://example.com\n")

  private def agreesAfterRandomEdits(pieces: Vector[String], seeds: Range): Assertion =
    def piece(random: Random): String = pieces(random.nextInt(pieces.size))
    seeds.foreach { seed =>
      val random  = new Random(seed)
      val initial = Rope(Vector.fill(30)(piece(random)).mkString)
      (1 to 60).foldLeft((initial, MarkdownHeadingIndex.build(initial))) {
        case ((content, index), _) =>
          val weight = content.weight
          val edited = random.nextInt(3) match
            case 0 => insertedAt(content, random.nextInt(weight + 1), piece(random))
            case 1 =>
              val start = random.nextInt(weight + 1)
              deleted(content, start, (start + 1 + random.nextInt(40)).min(weight))
            case _ => insertedAt(content, random.nextInt(weight + 1), Vector.fill(8)(piece(random)).mkString)
          val next = index.updatedTo(edited)
          withClue(
            s"seed $seed: ${content.collect().replace("\n", "\\n")} => ${edited.collect().replace("\n", "\\n")} "
          )(agreesWithFullParse(next, edited))
          (edited, next)
      }
    }
    succeed

  it should "agree with a full parse after random inserts, deletes and pastes" in
    agreesAfterRandomEdits(safePieces, 1 to 60)

  it should "agree with a full parse after random edits to text with constructs that span blank lines" in
    agreesAfterRandomEdits(hazardousPieces, 100 to 130)

  "an update given the edit's change" should "read the changed lines from the change rather than comparing the ropes" in {
    val before  = Rope(novel(chapters = 300))
    val index   = MarkdownHeadingIndex.build(before)
    val from    = before.collect().indexOf("# Chapter 100\n")
    val until   = before.collect().indexOf("# Chapter 130\n")
    val retyped = before.sliceString(from, until).replace("Chapter 110", "Chapter 11O")
    val after   = Rope(before.sliceString(0, from) + retyped + before.sliceString(until, before.weight))
    val change  = ChangeSet.single(before.weight, from, until, retyped)

    val next = index.updatedTo(after, Some(change))

    next.parsedLines should be > 100
    agreesWithFullParse(next, after)
  }

  "a document's heading index" should "advance across a recorded edit by that edit, not by comparing the texts" in {
    val before   = Rope(novel(chapters = 300))
    val plain    = Buffer.fromString(BufferId(901), "")
    val markdown = plain.document.copy(language = Some(LanguageId.Markdown)).withContent(before)
    markdown.markdownHeadings.parsedLines should be > 1_000

    val from    = before.collect().indexOf("# Chapter 100\n")
    val until   = before.collect().indexOf("# Chapter 130\n")
    val retyped = before.sliceString(from, until).replace("Chapter 110", "Chapter 11O")
    val after   = Rope(before.sliceString(0, from) + retyped + before.sliceString(until, before.weight))
    val edited  = markdown.edited(after, ChangeSet.single(before.weight, from, until, retyped))

    edited.markdownHeadings.parsedLines should be > 100
    agreesWithFullParse(edited.markdownHeadings, after)
  }

  "a document's heading index" should "give the headings of a buffer edited since they were last read" in {
    val plain    = Buffer.fromString(BufferId(900), "# One\n\nprose\n")
    val markdown = plain.copy(document = plain.document.copy(language = Some(LanguageId.Markdown)))
    markdown.document.markdownHeadings.headings.map(_.name) shouldBe List("One")

    val edited =
      markdown.copy(document = markdown.document.copy(content = insertedAt(markdown.document.content, 13, "\n# Two\n")))
    edited.document.markdownHeadings.headings.map(_.name) shouldBe List("One", "Two")
  }
