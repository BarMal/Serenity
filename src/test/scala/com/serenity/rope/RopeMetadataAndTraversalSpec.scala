package com.serenity.rope

import com.serenity.rope.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeMetadataAndTraversalSpec extends AnyFlatSpec with Matchers:

  extension (result: Option[Rope])
    // For test call sites where the operation is expected to succeed; failing loudly here (rather than defaulting
    // to some rope) keeps a regression that makes insert/delete/replace start failing from being masked as a
    // silently-wrong result.
    private def orFail: Rope = result.getOrElse(fail("expected the rope operation to succeed"))

  trait RopeSpecScope:
    given balance: Balance =
      Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 5)

  "Rope" should "handle line operations correctly" in new ChunkedRopeSpecScope:
    val multiline = Rope("line1\nline2\nline3\n")

    multiline.lineCount shouldBe 4 // 3 lines + 1 for final newline
    multiline.getLine(0) shouldBe Some("line1")
    multiline.getLine(1) shouldBe Some("line2")
    multiline.getLine(2) shouldBe Some("line3")
    multiline.getLine(3) shouldBe Some("")
    multiline.getLine(4) shouldBe None
    multiline.getLine(-1) shouldBe None

  it should "maintain newline metadata across rope operations" in new ChunkedRopeSpecScope:
    val initial = Rope("alpha\nbeta\ngamma")

    initial.newlineCount.shouldBe(2)
    initial.lastLineLength.shouldBe("gamma".length)
    initial.endsWithNewline.shouldBe(false)
    initial.lineCount.shouldBe(3)

    val concatenated = Rope("alpha\n").concat(Rope("beta\ngamma"))
    concatenated.newlineCount.shouldBe(2)
    concatenated.lastLineLength.shouldBe("gamma".length)
    concatenated.endsWithNewline.shouldBe(false)
    concatenated.lineCount.shouldBe(3)

    val inserted = Rope("alphagamma").insert("alpha".length, "\nbeta\n").orFail
    inserted.collect() shouldBe "alpha\nbeta\ngamma"
    inserted.newlineCount.shouldBe(2)
    inserted.lastLineLength.shouldBe("gamma".length)
    inserted.endsWithNewline.shouldBe(false)
    inserted.lineCount.shouldBe(3)

    val deleted = inserted.deleteRight("alpha".length, "\nbeta\n".length).orFail
    deleted.collect() shouldBe "alphagamma"
    deleted.newlineCount.shouldBe(0)
    deleted.lastLineLength.shouldBe("alphagamma".length)
    deleted.endsWithNewline.shouldBe(false)
    deleted.lineCount.shouldBe(1)

    val rebuilt = concatenated.rebuild
    rebuilt.collect() shouldBe "alpha\nbeta\ngamma"
    rebuilt.newlineCount.shouldBe(2)
    rebuilt.lastLineLength.shouldBe("gamma".length)
    rebuilt.endsWithNewline.shouldBe(false)
    rebuilt.lineCount.shouldBe(3)

    val trailingNewline = Rope("alpha\n").concat(Rope("beta\n"))
    trailingNewline.newlineCount.shouldBe(2)
    trailingNewline.lastLineLength.shouldBe(0)
    trailingNewline.endsWithNewline.shouldBe(true)
    trailingNewline.lineCount.shouldBe(3)

  it should "count words and non-whitespace characters for an empty rope" in new RopeSpecScope:
    Rope.empty.wordCount.shouldBe(0)
    Rope.empty.nonWhitespaceCount.shouldBe(0)

  it should "count words and non-whitespace characters for a single word" in new RopeSpecScope:
    val rope = Rope("hello")
    rope.wordCount.shouldBe(1)
    rope.nonWhitespaceCount.shouldBe(5)

  it should "count words and non-whitespace characters across multi-paragraph text" in new RopeSpecScope:
    val rope = Rope("The quick brown fox\n\njumps over  the lazy dog.\n")
    rope.wordCount.shouldBe(9)
    rope.nonWhitespaceCount.shouldBe("Thequickbrownfoxjumpsoverthelazydog.".length)

  it should "count unicode and CJK content as maximal non-whitespace runs" in new RopeSpecScope:
    val rope = Rope("café naïve 你好世界 emoji🎉test")
    rope.wordCount.shouldBe(4)
    rope.nonWhitespaceCount.shouldBe(rope.collect().count(!_.isWhitespace))

  it should "count punctuation-only content as words" in new RopeSpecScope:
    val rope = Rope("... !!! ,,,")
    rope.wordCount.shouldBe(3)
    rope.nonWhitespaceCount.shouldBe(9)

  it should "maintain word metadata incrementally across rope operations" in new ChunkedRopeSpecScope:
    val initial = Rope("alpha beta gamma")
    initial.wordCount.shouldBe(3)
    initial.nonWhitespaceCount.shouldBe("alphabetagamma".length)

    // A boundary split between two leaves must not double- or under-count the word straddling the join.
    val concatenated = Rope("alpha be").concat(Rope("ta gamma"))
    concatenated.collect() shouldBe "alpha beta gamma"
    concatenated.wordCount.shouldBe(3)
    concatenated.nonWhitespaceCount.shouldBe("alphabetagamma".length)

    val inserted = concatenated.insert("alpha beta".length, "-word").orFail
    inserted.collect() shouldBe "alpha beta-word gamma"
    inserted.wordCount.shouldBe(3)

    val deleted = inserted.deleteRight("alpha beta".length, "-word".length).orFail
    deleted.collect() shouldBe "alpha beta gamma"
    deleted.wordCount.shouldBe(3)
    deleted.nonWhitespaceCount.shouldBe("alphabetagamma".length)

    val rebuilt = concatenated.rebuild
    rebuilt.wordCount.shouldBe(3)
    rebuilt.nonWhitespaceCount.shouldBe("alphabetagamma".length)

  it should "keep an edit's word-count update proportional to tree depth, not document size" in new ChunkedRopeSpecScope:
    val words  = (1 to 5000).map(i => s"word$i").mkString(" ")
    val large  = Rope(words)
    val edited = large.insert(0, "prefix ").orFail

    edited.wordCount.shouldBe(large.wordCount + 1)
    // Only the spine down to the leftmost leaf should have been touched by the insert -- if word counting forced a
    // full rescan this would still pass functionally, but the depth bound documents the intent the perf spec
    // (WordStatisticsPerformanceSpec) exercises with real timings on a much larger buffer.
    edited.height should be <= large.height + 2

  it should "resolve line and column offsets through rope line traversal" in new ChunkedRopeSpecScope:
    val multiline = Rope("alpha\nbeta\ngamma")

    multiline.lineColumnToOffset(0, 0) shouldBe 0
    multiline.lineColumnToOffset(0, 3) shouldBe 3
    multiline.lineColumnToOffset(0, 20) shouldBe 5
    multiline.lineColumnToOffset(1, 0) shouldBe 6
    multiline.lineColumnToOffset(1, 2) shouldBe 8
    multiline.lineColumnToOffset(2, 5) shouldBe multiline.weight
    multiline.lineColumnToOffset(20, 0) shouldBe multiline.weight
    multiline.lineColumnToOffset(-1, -4) shouldBe 0

  it should "treat line and column conversions as UTF-16 code unit indexes for grapheme content" in new ChunkedRopeSpecScope:
    val content         = "a🙂b\ncafé!"
    val rope            = Rope(content)
    val secondLineStart = "a🙂b\n".length

    rope.lineColumnToOffset(0, 0) shouldBe 0
    rope.lineColumnToOffset(0, 1) shouldBe 1
    rope.lineColumnToOffset(0, 2) shouldBe 2
    rope.lineColumnToOffset(0, 3) shouldBe 3
    rope.lineColumnToOffset(0, 4) shouldBe 4

    rope.offsetToLineColumn(1) shouldBe (0, 1)
    rope.offsetToLineColumn(2) shouldBe (0, 2)
    rope.offsetToLineColumn(3) shouldBe (0, 3)

    rope.lineColumnToOffset(1, 3) shouldBe secondLineStart + 3
    rope.lineColumnToOffset(1, 4) shouldBe secondLineStart + 4
    rope.lineColumnToOffset(1, 5) shouldBe secondLineStart + 5

    rope.offsetToLineColumn(secondLineStart + 3) shouldBe (1, 3)
    rope.offsetToLineColumn(secondLineStart + 4) shouldBe (1, 4)
    rope.offsetToLineColumn(secondLineStart + 5) shouldBe (1, 5)

  it should "extract string slices without materialising the whole rope" in new ChunkedRopeSpecScope:
    val rope = Rope("alpha\nbeta\ngamma")

    rope.sliceString(0, 5) shouldBe "alpha"
    rope.sliceString(6, 10) shouldBe "beta"
    rope.sliceString(3, 14) shouldBe "ha\nbeta\ngam"
    rope.sliceString(-4, 99) shouldBe "alpha\nbeta\ngamma"
    rope.sliceString(8, 8) shouldBe ""

  it should "resolve offsets to line and column positions without collecting skipped branches" in new ChunkedRopeSpecScope:
    val skippedContent = (1 to 200).map(index => s"skip-$index").mkString("", "\n", "\n")
    val skippedBranch  = ExplodingIndexRope(Rope(skippedContent))
    val targetBranch   = Rope("target-line\n")
    val rope           = Node(skippedBranch, targetBranch)

    rope.offsetToLineColumn(skippedBranch.weight + 6) shouldBe (skippedBranch.newlineCount, 6)

  it should "read line helpers across many leaves" in new ChunkedRopeSpecScope:
    val content = (1 to 200).map(index => s"line-$index").mkString("\n")
    val rope    = Rope(content)

    rope.lineCount shouldBe 200
    rope.getLine(0) shouldBe Some("line-1")
    rope.getLine(149) shouldBe Some("line-150")
    rope.getLine(199) shouldBe Some("line-200")
    rope.lineColumnToOffset(149, 4) shouldBe content.indexOf("line-150") + 4

  it should "skip preceding rope branches when resolving later lines" in new ChunkedRopeSpecScope:
    val skippedContent = (1 to 200).map(index => s"skip-$index").mkString("", "\n", "\n")
    val skippedBranch  = ExplodingIndexRope(Rope(skippedContent))
    val targetBranch   = Rope("target-line\n")
    val rope           = Node(skippedBranch, targetBranch)
    val targetLine     = skippedBranch.newlineCount

    rope.getLine(targetLine) shouldBe Some("target-line")
    rope.lineColumnToOffset(targetLine, 6) shouldBe skippedBranch.weight + 6

  it should "search without materialising the whole rope" in new ChunkedRopeSpecScope:
    val content = "alpha needle\nbeta\nneedle gamma"
    val rope    = NonCollectingIndexedRope(Rope(content))

    rope.searchAll("needle") shouldBe List(content.indexOf("needle"), content.lastIndexOf("needle"))
    rope.searchAll("missing") shouldBe Nil

  it should "read sequential lines without materialising the whole rope" in new ChunkedRopeSpecScope:
    val content = "alpha\nbeta\ngamma\n"
    val rope    = NonCollectingIndexedRope(Rope(content))

    rope.linesFrom(0, 10) shouldBe Vector("alpha", "beta", "gamma", "")
    rope.linesFrom(1, 2) shouldBe Vector("beta", "gamma")
    rope.linesIteratorFrom(2).take(2).toVector shouldBe Vector(2 -> "gamma", 3 -> "")
    rope.linesFrom(4, 1) shouldBe Vector.empty

  it should "traverse leaf chunks directly for visible lines and search" in new ChunkedRopeSpecScope:
    val rope: Rope = Node(
      Node(new IndexForbiddenLeaf("alpha\nbe"), new IndexForbiddenLeaf("ta\n")),
      new IndexForbiddenLeaf("🙂🙂")
    )

    rope.sliceString(3, 9) shouldBe "ha\nbet"
    rope.linesFrom(1, 2) shouldBe Vector("beta", "🙂🙂")
    rope.searchAll("a\nb") shouldBe List(4)
    rope.searchAll("🙂") shouldBe List(11, 13)
    rope.searchAll("🙂🙂🙂") shouldBe Nil
    rope.searchAll("aa") shouldBe Nil

  trait ChunkedRopeSpecScope:
    given balance: Balance =
      Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  // `Rope` is sealed, so this can no longer extend it directly; it extends the still-open `Leaf` purely to satisfy
  // the type system when embedded as a `Node` child, seeded with the delegate's own text so a stray `case
  // Leaf(value)` match elsewhere at least reads the right characters. Every metadata accessor still forwards to
  // `delegate` rather than using anything inherited from `Leaf`, so callers that go through the `Rope` interface
  // (not a `Leaf`/`Node` pattern match) see this as an opaque, non-Leaf, non-Node rope exactly as before sealing.
  //
  // One guarantee this test double could make before sealing is now narrower: sealing means we can no longer
  // construct a *genuine* third `Rope` implementation, so production helpers that pattern-match `case Leaf(v)`
  // directly on a rope value (rather than calling its virtual methods) -- `Rope.leafValues`, `lineColumnToOffsetIn`,
  // `offsetToLineColumnIn`, `chunksInRange` (and so `getLine`/`linesFrom`/`linesIteratorFrom`/`searchAll`) -- would
  // now match this double structurally and take the Leaf fast path instead of the guarded `index`/`collect`
  // override. `getLine`/`lineColumnToOffset` (used by the "skip preceding rope branches" test below) are unaffected
  // because the traversal never recurses into this delegate's branch -- the enclosing `Node` dispatches purely on
  // `weight`/`newlineCount`/`endsWithNewline`, all ordinary virtual calls. A future test that pattern-matches into
  // this specific node would no longer be caught by the guard.
  final class ExplodingIndexRope(delegate: Rope)(using Balance) extends Leaf(delegate.collect()):
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override val newlineCount: Int =
      delegate.newlineCount

    override val lastLineLength: Int =
      delegate.lastLineLength

    override val endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def index(i: Int): Option[Char] =
      throw AssertionError("line traversal should skip this branch")

    override def collect(): String =
      throw AssertionError("line traversal should not materialise this branch")

  object ExplodingIndexRope:
    def apply(delegate: Rope)(using Balance): ExplodingIndexRope = new ExplodingIndexRope(delegate)

  // See `ExplodingIndexRope` above for why this extends `Leaf` rather than `Rope`, and for the guarantee that
  // narrows as a result. `searchAll`/`linesFrom`/`linesIteratorFrom` route through `Rope`'s private `chunksInRange`,
  // which pattern-matches `case Leaf(value)` directly -- unavoidable once this must satisfy `Leaf` -- so those three
  // are overridden here to forward straight to `delegate` rather than silently falling through the Leaf fast path
  // with a materialised value.
  final class NonCollectingIndexedRope(delegate: Rope)(using Balance) extends Leaf(delegate.collect()):
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override val newlineCount: Int =
      delegate.newlineCount

    override val lastLineLength: Int =
      delegate.lastLineLength

    override val endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def searchAll(term: String): List[Int] =
      delegate.searchAll(term)

    override def linesFrom(lineIndex: Int, maxLines: Int): Vector[String] =
      delegate.linesFrom(lineIndex, maxLines)

    override def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
      delegate.linesIteratorFrom(lineIndex)

    override def collect(): String =
      throw AssertionError("search should not materialise the whole rope")

  object NonCollectingIndexedRope:
    def apply(delegate: Rope)(using Balance): NonCollectingIndexedRope = new NonCollectingIndexedRope(delegate)

  final class IndexForbiddenLeaf(value: String)(using Balance) extends Leaf(value):
    override def index(i: Int): Option[Char] =
      throw AssertionError("sequential traversal should not index leaves character by character")
