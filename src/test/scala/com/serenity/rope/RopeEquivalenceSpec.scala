package com.serenity.rope

import scala.util.Random

import com.serenity.text.TextCounts
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Random edit sequences applied to a rope and to a plain `String` in step (#1900).
  *
  * After every edit, every public query the rope offers is compared with the same question asked of the `String`, so
  * the spec states what a rope must do without saying how its tree is laid out. It is the safety net for replacing the
  * tree: it must pass unchanged against any implementation of the public API. The tiny-leaf configuration forces deep
  * trees and many leaf boundaries out of a few hundred characters; the default one exercises the real leaf size.
  */
class RopeEquivalenceSpec extends AnyFlatSpec with Matchers:

  final private case class Fixture(label: String, balance: Balance, initialChars: Int, largeInsertChars: Int)

  final private case class Model(rope: Rope, text: String)

  private val fixtures = Vector(
    Fixture("default leaves", Balance.default, 6000, 12000),
    Fixture("tiny leaves", Balance(weightBalance = 8, heightBalance = 1, leafChunkSize = 12), 800, 600)
  )

  private val seeds = 1 to 6
  private val steps = 40

  private val tokens = Vector(
    "alpha",
    "beta",
    "gamma",
    " ",
    " ",
    "\n",
    "\n\n",
    "-",
    "well-known",
    "世界",
    "😀",
    "👨‍👩‍👧",
    "é",
    ".",
    "z" * 40
  )

  private val rawTokens = tokens ++ Vector("\r\n", "\r")

  private def normalized(raw: String): String = raw.replace("\r\n", "\n").replace('\r', '\n')

  private def randomText(random: Random, chars: Int, from: Vector[String] = rawTokens): String =
    Iterator
      .unfold(0) { length =>
        Option.when(length < chars) {
          val token = from(random.nextInt(from.size))
          (token, length + token.length)
        }
      }
      .mkString

  private def must(result: Option[Rope], what: String): Rope =
    result.getOrElse(fail(s"$what was rejected for an in-range argument"))

  private def sampledOffsets(random: Random, length: Int): Vector[Int] =
    val spread = Vector.fill(40)(random.nextInt(length + 1))
    (Vector(0, 1, length - 1, length) ++ spread).filter(offset => offset >= 0 && offset <= length).distinct.sorted

  private def sampledLines(random: Random, lineCount: Int): Vector[Int] =
    if lineCount <= 40 then Vector.range(0, lineCount)
    else (Vector(0, lineCount - 1) ++ Vector.fill(30)(random.nextInt(lineCount))).distinct.sorted

  private def lineStartsOf(text: String): Vector[Int] =
    0 +: text.indices.filter(text.charAt(_) == '\n').map(_ + 1).toVector

  private def occurrences(text: String, term: String): List[Int] =
    List.unfold(0)(from => Option(text.indexOf(term, from)).filter(_ >= 0).map(at => (at, at + term.length)))

  private def expectedChangedRange(before: String, after: String): Option[(Int, Int)] =
    if before == after then None
    else
      val bound  = math.min(before.length, after.length)
      val prefix = before.zip(after).takeWhile((a, b) => a == b).size
      val suffix =
        (0 until bound - prefix).takeWhile(i => before(before.length - 1 - i) == after(after.length - 1 - i)).size
      Some((prefix, after.length - suffix))

  // Queries -------------------------------------------------------------------------------------------------------

  private def assertContent(rope: Rope, text: String, random: Random)(using Balance): Unit =
    rope.collect() shouldBe text
    rope.toString shouldBe text
    rope.weight shouldBe text.length
    rope.contentEquals(text) shouldBe true
    rope.contentEquals(text + "x") shouldBe false
    rope.contentEquals(text.dropRight(1)) shouldBe text.isEmpty
    (rope == Rope(text)) shouldBe true
    (rope == Rope(text + "!")) shouldBe false
    rope.hashCode shouldBe text.hashCode
    rope.newlineCount shouldBe text.count(_ == '\n')
    rope.lineCount shouldBe text.count(_ == '\n') + 1
    rope.lastLineLength shouldBe text.length - text.lastIndexOf('\n') - 1
    rope.endsWithNewline shouldBe text.endsWith("\n")
    rope.index(-1) shouldBe None
    rope.index(text.length) shouldBe None
    sampledOffsets(random, text.length).filter(_ < text.length).foreach(at => rope.index(at) shouldBe Some(text(at)))
    val source = RopeCharacterSource(rope)
    source.length shouldBe text.length
    sampledOffsets(random, text.length).filter(_ < text.length).foreach(at => source.charAt(at) shouldBe text(at))
    RopeCharSequence(source).toString shouldBe text

  private def assertCounts(rope: Rope, text: String): Unit =
    val counts = TextCounts.of(text)
    rope.wordCount shouldBe counts.words
    rope.characterCount shouldBe counts.characters
    rope.nonWhitespaceCount shouldBe counts.nonWhitespaceCharacters
    rope.textSummary.counts shouldBe counts

  private def assertRanges(rope: Rope, text: String, random: Random): Unit =
    val offsets = sampledOffsets(random, text.length)
    for
      start <- offsets
      end   <- offsets.filter(_ >= start).take(6)
    do
      rope.sliceString(start, end) shouldBe text.substring(start, end)
      rope.slice(start, end).collect() shouldBe text.substring(start, end)
      val chunks = rope.chunksInRange(start, end).toVector
      chunks.map(_._2).mkString shouldBe text.substring(start, end)
      chunks.headOption.foreach((offset, _) => offset shouldBe start)
      chunks.sliding(2).foreach {
        case Vector((offset, chunk), (next, _)) => next shouldBe offset + chunk.length
        case _                                  => ()
      }
    rope.sliceString(-5, text.length + 5) shouldBe text
    rope.sliceString(5, 2) shouldBe ""
    rope.chunksInRange(-3, text.length + 3).map(_._2).mkString shouldBe text
    RopeChunks.of(rope).mkString shouldBe text

  private def assertLines(rope: Rope, text: String, random: Random): Unit =
    val lines   = text.split("\n", -1).toVector
    val sampled = sampledLines(random, lines.size)
    rope.getLine(-1) shouldBe None
    rope.getLine(lines.size) shouldBe None
    sampled.foreach(line => rope.getLine(line) shouldBe Some(lines(line)))
    sampled.foreach { from =>
      List(0, 1, 5, lines.size + 1).foreach(max => rope.linesFrom(from, max) shouldBe lines.drop(from).take(max))
      rope.linesIteratorFrom(from).toVector shouldBe (from until lines.size).map(line => line -> lines(line)).toVector
    }
    rope.linesIteratorFrom(-1).toVector shouldBe Vector.empty
    rope.linesIteratorFrom(lines.size).toVector shouldBe Vector.empty

  private def assertPositions(rope: Rope, text: String, random: Random): Unit =
    val starts = lineStartsOf(text)
    val ends   = starts.drop(1).map(_ - 1) :+ text.length

    def expectedOffset(line: Int, column: Int): Int =
      val target = math.max(0, line)
      if target >= starts.size then text.length
      else starts(target) + math.min(math.max(0, column), ends(target) - starts(target))

    def expectedPosition(offset: Int): (Int, Int) =
      val clamped = math.max(0, math.min(offset, text.length))
      val line    = starts.count(_ <= clamped) - 1
      (line, clamped - starts(line))

    val lines = sampledLines(random, starts.size)
    for
      line   <- lines ++ Vector(-3, starts.size, starts.size + 4)
      column <- Vector(-1, 0, 1, 7, 100000) ++ Option.when(line >= 0 && line < starts.size)(ends(line) - starts(line))
    do rope.lineColumnToOffset(line, column) shouldBe expectedOffset(line, column)

    val offsets = Vector(-5, text.length + 3) ++ sampledOffsets(random, text.length)
    offsets.foreach(offset => rope.offsetToLineColumn(offset) shouldBe expectedPosition(offset))
    val ascending = (offsets ++ offsets.take(8)).sorted
    rope.offsetsToLineColumns(ascending) shouldBe ascending.map(expectedPosition)
    rope.offsetsToLineColumns(Vector.empty) shouldBe Vector.empty

  private def assertSearch(rope: Rope, text: String, random: Random): Unit =
    val present = Vector.fill(6) {
      val start = random.nextInt(text.length + 1)
      text.substring(start, math.min(text.length, start + 1 + random.nextInt(4)))
    }
    (present.filter(_.nonEmpty) ++ Vector("qqqq", text + "tail")).foreach { term =>
      rope.searchAll(term) shouldBe occurrences(text, term)
    }
    rope.searchAll("") shouldBe Nil

  private def assertEquivalent(rope: Rope, text: String, random: Random)(using Balance): Unit =
    assertContent(rope, text, random)
    assertCounts(rope, text)
    assertRanges(rope, text, random)
    assertLines(rope, text, random)
    assertPositions(rope, text, random)
    assertSearch(rope, text, random)

  // Edits ---------------------------------------------------------------------------------------------------------

  private def inserted(model: Model, at: Int, raw: String): Model =
    Model(must(model.rope.insert(at, raw), "insert"), model.text.patch(at, normalized(raw), 0))

  private def deleted(model: Model, start: Int, end: Int): Model =
    Model(must(model.rope.delete(start, end), "delete"), model.text.patch(start, "", end - start))

  private def deletedRight(model: Model, start: Int, count: Int): Model =
    Model(must(model.rope.deleteRight(start, count), "deleteRight"), model.text.patch(start, "", count))

  private def deletedLeft(model: Model, start: Int, count: Int): Model =
    val clamped = math.min(start, model.text.length)
    val actual  = math.min(count, clamped)
    Model(model.rope.deleteLeft(start, count), model.text.patch(clamped - actual, "", actual))

  private def replaced(model: Model, at: Int, char: Char): Model =
    Model(must(model.rope.replace(at, char), "replace"), model.text.patch(at, char.toString, 1))

  private def spliceAround(random: Random, model: Model)(using Balance): (String, Model) =
    val at            = random.nextInt(model.text.length + 1)
    val (left, right) = model.rope.splitAt(at).getOrElse(fail("splitAt rejected an in-range position"))
    left.collect() shouldBe model.text.take(at)
    right.collect() shouldBe model.text.drop(at)
    left.weight shouldBe at
    assertEquivalent(left, model.text.take(at), random)
    assertEquivalent(right, model.text.drop(at), random)
    random.nextInt(4) match
      case 0 => ("split, keep left", Model(left, model.text.take(at)))
      case 1 => ("split, keep right", Model(right, model.text.drop(at)))
      case _ => ("split and rejoin", Model(left.concat(right), model.text))

  private def edit(random: Random, model: Model, fixture: Fixture)(using Balance): (String, Model) =
    val length        = model.text.length
    def position: Int = random.nextInt(length + 1)
    def span(maxLength: Int): (Int, Int) =
      val start = position
      (start, math.min(length, start + random.nextInt(maxLength + 1)))
    def large = randomText(random, fixture.largeInsertChars)

    if length < 30 then ("large insert", inserted(model, position, large))
    else if length > fixture.initialChars * 4 then
      val start = random.nextInt(length / 2)
      ("range delete", deleted(model, start, start + length / 2))
    else
      random.nextInt(18) match
        case 0 | 1 | 2 => ("insert", inserted(model, position, randomText(random, 1 + random.nextInt(12))))
        case 3         => ("insert at start", inserted(model, 0, randomText(random, 1 + random.nextInt(6))))
        case 4         => ("insert at end", inserted(model, length, randomText(random, 1 + random.nextInt(6))))
        case 5 | 6 =>
          val (start, end) = span(6)
          ("delete", deleted(model, start, end))
        case 7 =>
          val (start, end) = span(length / 3)
          ("range delete", deleted(model, start, end))
        case 8 => ("large insert", inserted(model, position, large))
        case 9 =>
          val at = random.nextInt(length)
          ("replace", replaced(model, at, "ab\n-".charAt(random.nextInt(4))))
        case 10 =>
          val other = randomText(random, 1 + random.nextInt(fixture.largeInsertChars / 4))
          ("concat after", Model(model.rope.concat(Rope(other)), model.text + normalized(other)))
        case 11 =>
          val other = randomText(random, 1 + random.nextInt(fixture.largeInsertChars / 4))
          ("concat before", Model(Rope(other).concat(model.rope), normalized(other) + model.text))
        case 12 | 13 => spliceAround(random, model)
        case 14 =>
          val (start, count) = (position, random.nextInt(8))
          ("deleteRight", deletedRight(model, start, math.min(count, length - start)))
        case 15 => ("deleteLeft", deletedLeft(model, position, 1 + random.nextInt(8)))
        case 16 =>
          val drop = random.nextInt(math.max(1, length / 4))
          if random.nextBoolean() then ("dropLeft", Model(model.rope.dropLeft(drop), model.text.drop(drop)))
          else ("dropRight", Model(model.rope.dropRight(drop), model.text.dropRight(drop)))
        case _ => replacedAll(random, model)

  private def replacedAll(random: Random, model: Model): (String, Model) =
    val start       = random.nextInt(model.text.length)
    val term        = model.text.substring(start, math.min(model.text.length, start + 1 + random.nextInt(3)))
    val replacement = randomText(random, 1 + random.nextInt(4), tokens)
    ("replaceAll", Model(model.rope.replaceAll(term, replacement), model.text.replace(term, replacement)))

  private def runSequence(fixture: Fixture, seed: Int)(using Balance): Unit =
    val random  = Random(seed * 7919L + fixture.balance.leafChunkSize)
    val initial = normalized(randomText(random, fixture.initialChars))
    val start   = Model(Rope(initial), initial)
    assertEquivalent(start.rope, start.text, random)
    val _ = (1 to steps).foldLeft(start) { (model, step) =>
      val (name, next) = edit(random, model, fixture)
      withClue(s"${fixture.label}, seed $seed, step $step ($name): ") {
        assertEquivalent(next.rope, next.text, random)
        RopeDiff.changedOffsetRange(model.rope, next.rope) shouldBe expectedChangedRange(model.text, next.text)
      }
      next
    }

  // Specs ---------------------------------------------------------------------------------------------------------

  "A rope" should "agree with a plain String after every edit of random edit sequences" in {
    for
      fixture <- fixtures
      seed    <- seeds
    do runSequence(fixture, seed)(using fixture.balance)
  }

  it should "agree with the String for an empty document" in {
    given Balance = Balance.default
    val random    = Random(11)
    assertEquivalent(Rope.empty, "", random)
    assertEquivalent(Rope(""), "", random)
    Rope.empty.splitAt(0).map((l, r) => (l.collect(), r.collect())) shouldBe Some(("", ""))
  }

  it should "normalise CR and CRLF in inserted text at the API boundary" in
    fixtures.foreach { fixture =>
      given Balance = fixture.balance
      val rope      = Rope("one\ntwo")
      must(rope.insert(4, "a\r\nb\rc"), "insert").collect() shouldBe "one\na\nb\nctwo"
      Rope("x\r\ny\rz").collect() shouldBe "x\ny\nz"
    }

  it should "reject out-of-range positions and clamp overlong counts as documented" in
    fixtures.foreach { fixture =>
      given Balance = fixture.balance
      List("short", "x" * 150).foreach { text =>
        val rope = Rope(text)
        val end  = text.length
        rope.insert(-1, "q") shouldBe None
        rope.insert(end + 1, "q") shouldBe None
        rope.insert(end, "q").map(_.collect()) shouldBe Some(text + "q")
        rope.splitAt(-1) shouldBe None
        rope.splitAt(end + 1) shouldBe None
        rope.replace(-1, 'q') shouldBe None
        rope.replace(end, 'q') shouldBe None
        rope.deleteRight(-1, 2) shouldBe None
        rope.deleteRight(end + 1, 2) shouldBe None
        rope.deleteRight(end + 1, 0).map(_.collect()) shouldBe Some(text)
        rope.deleteRight(end - 2, 10).map(_.collect()) shouldBe Some(text.dropRight(2))
        val (high, low) = (math.min(10, end), math.min(5, end))
        rope.delete(high, low).map(_.collect()) shouldBe Some(text.patch(low, "", high - low))
        rope.deleteLeft(end + 20, 3).collect() shouldBe text.dropRight(3)
        rope.deleteLeft(0, 3).collect() shouldBe text
        rope.deleteLeft(-5, 3).collect() shouldBe text
        rope.deleteLeft(end, 0).collect() shouldBe text
        rope.dropLeft(end + 9).collect() shouldBe ""
        rope.dropRight(end + 9).collect() shouldBe ""
      }
    }
end RopeEquivalenceSpec
