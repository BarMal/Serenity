package com.serenity.state.manager

import com.serenity.richtext.{ParagraphRole, RichTextParagraph}
import com.serenity.state.manager.ColumnPagingFixture.{
  bodyFollower,
  dropCapFollower,
  heading,
  leadAfterFillers,
  rowId,
  sameTop
}
import com.serenity.state.models.{TextVisualLine, Viewport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A heading is kept with what follows it: a column never ends on a heading when the row after it could start the next
  * column, a run of headings travels together, and a run that could not fit with its follower in an empty column is not
  * pushed again and again. Every expectation is derived from the row heights the font measures: the document puts
  * `fillers` one-row paragraphs before the headings and the specs sweep `fillers` over every distance from the bottom
  * of the column, so some count lands the headings on the edge whichever font the platform maps to.
  */
class KeepWithNextColumnPagingSpec extends AnyFlatSpec with Matchers:

  private val oneHeading   = List(heading(2, "Chapter"))
  private val twoHeadings  = List(heading(1, "Part"), heading(2, "Chapter"))
  private val sweptFixture = (scale: Int, columns: Int) =>
    ColumnPagingFixture(scale, columns, source = ColumnPagingFixture.sweptHeadings)

  /** A document laid out as one endless column, and what each of its paragraphs stacks to in a column. */
  private final case class Measured(rows: Vector[TextVisualLine], lineHeightPx: Int):

    def bandPx(row: TextVisualLine): Int =
      row.spaceBeforePx + math.max(lineHeightPx, row.heightPx) + row.reservedBelowPx + row.spaceAfterPx

    def rowsOf(line: Int): Vector[TextVisualLine] = rows.filter(_.bufferLine == line)

    def stackPx(first: Int, count: Int): Int = (first until first + count).flatMap(rowsOf).map(bandPx).sum

    /** What a paragraph takes to be placed in a column that holds something: its rows, or for a drop cap the rows its
      * glyph covers, in either case without the room below the last row.
      */
    def placePx(line: Int): Int =
      val covered = glyphSpan(line)
      covered.map(bandPx).sum - covered.last.spaceAfterPx

    def firstRowPx(line: Int): Int =
      val home = rowsOf(line).head
      bandPx(home) - home.spaceAfterPx

    /** The rows of `line` its drop cap's glyph covers; its first row alone when it has none. */
    def glyphSpan(line: Int): Vector[TextVisualLine] =
      val own = rowsOf(line)
      val glyphBottomPx = own.head.dropCapHeightPx
      if glyphBottomPx <= 0 then Vector(own.head)
      else
        val tops = own.scanLeft(0)((topPx, row) => topPx + (if row.heightPx > 0 then row.heightPx else lineHeightPx))
        own.zip(tops).takeWhile((_, topPx) => topPx < glyphBottomPx).map(_._1)

  private def measure(fixture: ColumnPagingFixture): Measured =
    val column = fixture.columnsAt(Viewport(0, 0, fixture.textColumns, 1), 1, rows = 100000).head
    Measured(column.visualLines, column.lineHeightPx)

  private def firstPage(fixture: ColumnPagingFixture): Vector[Vector[TextVisualLine]] =
    fixture.columnsAt(Viewport(0, 0, fixture.textColumns, 1)).map(_.visualLines)

  /** Where the headings of a document with `fillers` paragraphs before them landed, and what the measured heights say
    * about whether they fit.
    */
  private final case class Outcome(
      fillers: Int,
      headingColumn: Int,
      runFitsWithFollower: Boolean,
      firstHeadingFitsAlone: Boolean,
      runFitsWithFirstRowOnly: Boolean,
      runWhole: Boolean,
      followerBesideRun: Boolean
  ):
    def kept: Boolean = headingColumn == 0 && followerBesideRun
    def moved: Boolean = headingColumn == 1 && runWhole
    def asMeasured: Boolean = if runFitsWithFollower then kept else moved

  private def sweep(scale: Int, lead: List[RichTextParagraph], follower: RichTextParagraph): Vector[Outcome] =
    val oneFiller = measure(ColumnPagingFixture(scale, 2, source = leadAfterFillers(1, lead, follower))).stackPx(0, 1)
    val fitting   = ColumnPagingFixture(scale, 2, source = leadAfterFillers(1, lead, follower)).columnHeightPx / oneFiller
    (0 to fitting).toVector.map { fillers =>
      val fixture      = ColumnPagingFixture(scale, 2, source = leadAfterFillers(fillers, lead, follower))
      val measured     = measure(fixture)
      val used         = measured.stackPx(0, fillers)
      val run          = measured.stackPx(fillers, lead.size)
      val followerLine = fillers + lead.size
      val height       = fixture.columnHeightPx
      val columns      = firstPage(fixture)
      def inOneColumn(lines: Seq[Int]): Boolean =
        columns.exists(column => lines.forall(line => column.exists(_.bufferLine == line)))
      Outcome(
        fillers,
        headingColumn = columns.indexWhere(_.exists(_.bufferLine == fillers)),
        runFitsWithFollower = fillers == 0 || used + run + measured.placePx(followerLine) <= height,
        firstHeadingFitsAlone = fillers == 0 || used + measured.placePx(fillers) <= height,
        runFitsWithFirstRowOnly = fillers == 0 || used + run + measured.firstRowPx(followerLine) <= height,
        runWhole = inOneColumn(fillers until followerLine),
        followerBesideRun = inOneColumn(fillers to followerLine)
      )
    }

  for scale <- List(1, 2) do
    s"A heading near the bottom of a column at ${scale}x" should
      "move to the next column when the row after it would not fit beside it" in {
        val outcomes = sweep(scale, oneHeading, bodyFollower)
        outcomes.filterNot(_.asMeasured) shouldBe Vector.empty
        outcomes.exists(o => !o.runFitsWithFollower && o.firstHeadingFitsAlone) shouldBe true
      }

    it should "stay in its column, beside the row after it, when both fit" in {
      val stayed = sweep(scale, oneHeading, bodyFollower).filter(o => o.fillers > 0 && o.runFitsWithFollower)
      stayed should not be empty
      stayed.filterNot(_.kept) shouldBe Vector.empty
    }

    s"Consecutive headings at ${scale}x" should "move together to the next column with the row after them" in {
      val outcomes = sweep(scale, twoHeadings, bodyFollower)
      outcomes.filterNot(_.asMeasured) shouldBe Vector.empty
      outcomes.exists(o => !o.runFitsWithFollower && o.firstHeadingFitsAlone) shouldBe true
      outcomes.exists(o => o.fillers > 0 && o.runFitsWithFollower) shouldBe true
    }

    s"A heading before a drop cap at ${scale}x" should
      "move to the next column when the glyph's whole span would not fit beside it" in {
        val outcomes = sweep(scale, oneHeading, dropCapFollower)
        outcomes.filterNot(_.asMeasured) shouldBe Vector.empty
        outcomes.exists(o => !o.runFitsWithFollower && o.runFitsWithFirstRowOnly) shouldBe true
      }

    it should "keep the glyph's span in one column" in {
      val split = (0 to 12).filterNot { fillers =>
        val fixture = ColumnPagingFixture(scale, 2, source = leadAfterFillers(fillers, oneHeading, dropCapFollower))
        val span    = measure(fixture).glyphSpan(fillers + oneHeading.size)
        firstPage(fixture).exists(column => span.forall(row => column.exists(shown => rowId(shown) == rowId(row))))
      }
      split shouldBe empty
    }

    s"A run of headings that cannot fit with its follower in an empty column at ${scale}x" should
      "stand at the top of a column instead of being pushed on" in {
        val cases = for
          gridRows <- 2 to 9
          fillers  <- 0 to 3
        yield
          val fixture  = ColumnPagingFixture(scale, 2, gridRows, leadAfterFillers(fillers, twoHeadings, dropCapFollower))
          val measured = measure(fixture)
          val run      = measured.stackPx(fillers, twoHeadings.size)
          val oversized = run + measured.placePx(fillers + twoHeadings.size) > fixture.columnHeightPx
          val aloneFits = measured.stackPx(0, fillers) + run <= fixture.columnHeightPx
          (fixture, fillers, oversized, aloneFits)
        val oversize = cases.filter(_._3)
        oversize should not be empty
        oversize.exists(c => c._2 > 0 && c._4) shouldBe true

        val failures = oversize.flatMap { (fixture, fillers, _, _) =>
          val columns = fixture.pages.flatMap(_.columns).map(_.visualLines)
          val where   = s"gridRows ${fixture.gridRows}, $fillers fillers"
          List(
            Option.when(columns.flatten.map(rowId) != fixture.truth)(s"$where: rows differ from the document"),
            Option.when(columns.exists(_.isEmpty))(s"$where: an empty column"),
            Option.when(!columns.exists(_.headOption.map(rowId).contains((fillers, 0))))(
              s"$where: headings do not open a column"
            )
          ).flatten
        }
        failures.take(5) shouldBe Vector.empty
      }

  "A document that sweeps headings across every distance from a column's bottom" should
    "never end a column on a heading, and place every row once" in {
      val stranded = for
        scale   <- List(1, 2)
        columns <- List(1, 2, 3)
        fixture  = sweptFixture(scale, columns)
        headingLines =
          fixture.source.paragraphs.zipWithIndex.collect { case (paragraph, line) if isHeading(paragraph) => line }.toSet
        all = fixture.pages.flatMap(_.columns).map(_.visualLines)
        column <- all.dropRight(1)
        last   <- column.lastOption
        if headingLines.contains(last.bufferLine)
      yield s"${scale}x/$columns: a column ends on heading line ${last.bufferLine}"
      stranded.take(5) shouldBe empty
    }

  it should "place every row exactly once across its pages" in {
    val misplaced = for
      scale   <- List(1, 2)
      columns <- List(1, 2, 3)
      fixture = sweptFixture(scale, columns)
      if fixture.pages.flatMap(_.columns).flatMap(_.visualLines).map(rowId) != fixture.truth
    yield s"${scale}x/$columns"
    misplaced shouldBe empty
  }

  it should "anchor each row's page cold the same as walking onto it, with the cursor shown" in {
    val problems = for
      scale   <- List(1, 2)
      columns <- List(1, 2, 3)
      fixture = sweptFixture(scale, columns)
      walked  = fixture.truth.scanLeft(fixture.buffer)((walk, id) => fixture.placed(walk, id)).drop(1)
      (walk, id) <- walked.zip(fixture.truth)
      cold  = fixture.placed(fixture.buffer, id).viewport
      shown = fixture.columnsAt(cold).flatMap(_.visualLines).map(rowId)
      problem <- List(
        Option.when(!sameTop(walk.viewport, cold))(s"${scale}x/$columns: row $id walks to another page"),
        Option.when(!shown.contains(id))(s"${scale}x/$columns: row $id is not on its page")
      ).flatten
    yield problem
    problems.take(5) shouldBe empty
  }

  private def isHeading(paragraph: RichTextParagraph): Boolean =
    paragraph.role match
      case ParagraphRole.Heading(_) => true
      case _                        => false
