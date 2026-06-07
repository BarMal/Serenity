package com.serenity.rope

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.rope.*

class RopeSpec extends AnyFlatSpec with Matchers:

  "Rope" should "weight" in new RopeSpecScope:
    val c: Node = Node(Leaf("Hello "), Leaf("my "))
    c.weight shouldBe 9
    val g: Node = Node(Leaf("na"), Leaf("me i"))
    g.weight shouldBe 6
    val h: Node = Node(Leaf("s"), Leaf(" Barney"))
    h.weight shouldBe 8

    val d: Node = Node(g, h)
    d.weight shouldBe 14
    val b: Node = Node(c, d)
    b.weight shouldBe 23

    val a: Node = Node(b, Leaf(""))
    a.weight shouldBe 23

    a.collect() shouldBe "Hello my name is Barney"

  it should "concat" in new RopeSpecScope:
    Rope("Hello").concat(Rope(" world!")).collect() shouldBe "Hello world!"

  it should "balance" in new RopeSpecScope:
    val depth4: Node  = Node(Leaf("Deepest"), Leaf(""))
    val depth3a: Node = Node(depth4, Node(Leaf("Up1"), Leaf("")))
    val depth3b: Node = Node(Leaf("Up1a"), Leaf("Up1b"))
    val depth2a: Node = Node(depth3a, depth3b)
    val depth2b: Node = Node(Leaf("Up2"), Leaf(""))
    val root: Node    = Node(depth2a, depth2b)

    root.isHeightBalanced shouldBe false
    root.isWeightBalanced shouldBe false

    val rebuiltRoot: Rope = root.rebuild
    rebuiltRoot.isHeightBalanced shouldBe true
    rebuiltRoot.isWeightBalanced shouldBe true

    val rebalancedRoot: Rope = root.rebalance
    rebalancedRoot.isHeightBalanced shouldBe true
    rebalancedRoot.isWeightBalanced shouldBe true

  it should "index" in new RopeSpecScope:
    val a: Rope = Rope("Hello, my name is Barney")

    a.collect().zipWithIndex.map {
      case (char, index) =>
        a.index(index) shouldBe Some(char)
    }

    a.index(-1) shouldBe None
    a.index(a.weight + 1) shouldBe None

  it should "split" in new RopeSpecScope:
    val a: Rope = Rope("Hello, my name is Barney")

    List(0, 12, a.weight + 1).foreach { scenarioIndex =>
      a.splitAt(scenarioIndex).map { (l, r) =>
        val (el, er) = a.collect().splitAt(scenarioIndex)
        l.collect() shouldBe el
        r.collect() shouldBe er
      }
    }

  it should "insert" in new RopeSpecScope:
    Rope("Hello world!").insert(5, ",").collect() shouldBe "Hello, world!"

  it should "delete" in new RopeSpecScope:
    Rope("Hello, world!").deleteLeft(3, 2).collect() shouldBe "Hlo, world!"
    Rope("Hello, world!").deleteLeft(3, 12).collect() shouldBe "lo, world!"
    Rope("Hello, world!").deleteRight(13, 1).collect() shouldBe "Hello, world!"
    Rope("Hello, world!").deleteRight(13, -13).collect() shouldBe ""
    Rope("Hello, world!").deleteLeft(0, -13).collect() shouldBe ""

  it should "replace" in new RopeSpecScope:
    Rope("Hello, world!")
      .replace(5, '!')
      .replace(7, 'W')
      .collect() shouldBe "Hello! World!"

  it should "search for first occurrence" in new ChunkedRopeSpecScope:
    val lorem0: String =
      """Lorem ipsum dolor sit amet, consectetur adipiscing
         |elit, sed do eiusmod tempor incididunt ut labore et
         |dolore magna aliqua. Ut enim ad minim veniam, quis
         |nostrud exercitation ullamco laboris nisi ut aliquip
         |ex ea commodo consequat. Duis aute irure dolor in
         |reprehenderit in voluptate velit esse cillum dolore
         |eu fugiat nulla pariatur. Excepteur sint occaecat
         |cupidatat non proident, sunt in culpa qui officia
         |deserunt mollit anim id est laborum.""".stripMargin

    val rope: Rope = Rope(lorem0)

    val search0: String = "Lorem"
    rope.search(search0) shouldBe Some(0)

    val normalized      = lorem0.replace("\r\n", "\n")
    val search1: String = "laborum"
    rope.search(search1) shouldBe Some(normalized.indexOf(search1))

    val search2: String = "in culpa qui officia"
    rope.search(search2) shouldBe Some(normalized.indexOf(search2))

    val search3: String = "Doesn't exist in the body"
    rope.search(search3) shouldBe None

  it should "search for all occurrences" in new ChunkedRopeSpecScope:
    val text = "the cat sat on the mat with the rat"
    val rope = Rope(text)

    val results  = rope.searchAll("the")
    val expected = List(0, 15, 28) // All positions of "the"

    results should contain theSameElementsAs expected

  it should "find overlapping matches" in new ChunkedRopeSpecScope:
    val text = "aaaaaa"
    val rope = Rope(text)

    val results = rope.searchAll("aa")
    // Should find: positions 0, 1, 2, 3, 4
    results.length should be >= 5

  it should "search across leaf boundaries" in new ChunkedRopeSpecScope:
    // With leafChunkSize = 30, create a pattern that spans boundaries
    val text = "a" * 28 + "boundary" + "b" * 28
    val rope = Rope(text)

    rope.search("boundary") shouldBe Some(28)
    rope.searchAll("boundary") should contain(28)

  it should "handle repeated patterns" in new ChunkedRopeSpecScope:
    val text = "abc abc abc abc"
    val rope = Rope(text)

    val results = rope.searchAll("abc")
    results should contain theSameElementsAs List(0, 4, 8, 12)

  it should "handle patterns at boundaries" in new ChunkedRopeSpecScope:
    // Create text where pattern spans leaf boundary
    val part1 = "x" * 28 + "te"
    val part2 = "st" + "y" * 28
    val text  = part1 + part2
    val rope  = Rope(text)

    rope.search("test") shouldBe Some(28)

  it should "handle single character searches" in new ChunkedRopeSpecScope:
    val text = "abcabc"
    val rope = Rope(text)

    val results = rope.searchAll("a")
    results should contain theSameElementsAs List(0, 3)

  it should "handle search at end of rope" in new ChunkedRopeSpecScope:
    val text = "hello world test"
    val rope = Rope(text)

    rope.search("test") shouldBe Some(12)
    rope.searchAll("test") should contain(12)

  it should "handle empty search results" in new ChunkedRopeSpecScope:
    val text = "hello world"
    val rope = Rope(text)

    rope.search("xyz") shouldBe None
    rope.searchAll("xyz") shouldBe empty

  it should "handle case-sensitive searches" in new ChunkedRopeSpecScope:
    val text = "Hello hello HELLO"
    val rope = Rope(text)

    rope.searchAll("hello") should contain theSameElementsAs List(6)
    rope.searchAll("Hello") should contain theSameElementsAs List(0)
    rope.searchAll("HELLO") should contain theSameElementsAs List(12)

  it should "handle multiline searches" in new ChunkedRopeSpecScope:
    val text = "line1\npattern\nline3\npattern\nline5"
    val rope = Rope(text)

    val results = rope.searchAll("pattern")
    results.length shouldBe 2
    results should contain theSameElementsAs List(6, 20)

  it should "handle long patterns" in new ChunkedRopeSpecScope:
    val pattern = "this is a very long pattern that spans multiple chunks"
    val text    = "prefix " + pattern + " middle " + pattern + " suffix"
    val rope    = Rope(text)

    val results = rope.searchAll(pattern)
    results.length shouldBe 2

  it should "handle patterns with special characters" in new ChunkedRopeSpecScope:
    val text = "test\nwith\ttabs and spaces"
    val rope = Rope(text)

    rope.search("\n") shouldBe Some(4)
    rope.search("\t") shouldBe Some(9)
    rope.search(" ") shouldBe Some(14)

  it should "replaceAll correctly" in new ChunkedRopeSpecScope:
    val text = "the cat sat on the mat"
    val rope = Rope(text)

    val replaced = rope.replaceAll("the", "a")
    replaced.collect() shouldBe "a cat sat on a mat"

  it should "replaceAll with longer replacement" in new ChunkedRopeSpecScope:
    val text = "a b a b a"
    val rope = Rope(text)

    val replaced = rope.replaceAll("a", "long")
    replaced.collect() shouldBe "long b long b long"

  it should "handle large strings efficiently" in new RopeSpecScope:
    // Test with progressively larger strings to verify memory efficiency
    // Use a reasonable size that tests scalability without causing OOM
    val baseString = "Hello, this is a test string with some content. "

    // Test with strings of increasing size
    val sizes = List(1000, 10000, 100000) // Characters, not 50M to avoid OOM

    sizes.foreach { size =>
      val repetitions = size / baseString.length
      val largeText   = baseString * repetitions
      val rope        = Rope(largeText)

      // Verify rope structure remains efficient
      rope.weight shouldBe largeText.length
      rope.isWeightBalanced shouldBe true
      rope.isHeightBalanced shouldBe true

      // Test operations on large rope work correctly
      rope.index(0) shouldBe Some('H')
      if largeText.nonEmpty then rope.index(largeText.length - 1) should be(defined)
      rope.search("Hello") shouldBe Some(0)

      // Test that rebalancing works on large ropes
      val rebalanced = rope.rebalance
      rebalanced.isWeightBalanced shouldBe true
      rebalanced.collect() shouldBe largeText

      // Test rebuilding works without memory issues
      val rebuilt = rope.rebuild
      rebuilt.collect() shouldBe largeText
    }

  it should "simulate backspace correctly" in new RopeSpecScope:
    // Test basic backspace behavior: delete character to the left of cursor
    val text           = "Hello World"
    val rope           = Rope(text)
    val cursorPosition = 6 // At "W" in "Hello World"

    // Backspace should delete the " " character (1 char left of position 6)
    val result = rope.deleteLeft(cursorPosition, 1)
    result.collect() shouldBe "HelloWorld"

    // Test backspace at beginning (should not delete anything)
    val resultAtStart = rope.deleteLeft(0, 1)
    resultAtStart.collect() shouldBe text

    // Test multiple backspaces
    val multipleBackspace = rope.deleteLeft(cursorPosition, 2)
    multipleBackspace.collect() shouldBe "HellWorld"

    // Test backspace at end
    val resultAtEnd = rope.deleteLeft(text.length, 1)
    resultAtEnd.collect() shouldBe "Hello Worl"

  trait RopeSpecScope:
    given balance: Balance =
      Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 5)

  it should "maintain rope balance during many small insertions" in new ChunkedRopeSpecScope:
    // Simulate typing session with many small insertions
    val finalRope = (1 to 1000).foldLeft(Rope("")) { (rope, i) =>
      val newRope = rope.insert(rope.weight, s"char$i ")
      if i % 100 == 0 then
        // Periodically check balance
        newRope.isHeightBalanced shouldBe true
        newRope.isWeightBalanced shouldBe true
      newRope
    }

    // Final check
    finalRope.isHeightBalanced shouldBe true
    finalRope.isWeightBalanced shouldBe true
    finalRope.weight should be > 6000 // Rough check for content

  it should "handle deletion at various positions efficiently" in new ChunkedRopeSpecScope:
    val text = "The quick brown fox jumps over the lazy dog"
    val rope = Rope(text)

    // Test deletion at beginning - no chars to left of position 0
    val deleteStart = rope.deleteLeft(0, 4)
    deleteStart.collect() shouldBe text

    // Test deletion from position 4 - delete 4 chars to left (positions 0,1,2,3 = "The ")
    val deleteFromPos4 = rope.deleteLeft(4, 4)
    deleteFromPos4.collect() shouldBe "quick brown fox jumps over the lazy dog"

    // Test deletion at middle - delete 5 chars to left of position 20 (positions 15,16,17,18,19 = " fox ")
    val deleteMiddle = rope.deleteLeft(20, 5)
    deleteMiddle.collect() shouldBe "The quick brownjumps over the lazy dog"

    // Test deletion at end - delete 3 chars to left from end (positions 40,41,42 = "dog")
    val deleteEnd = rope.deleteLeft(rope.weight, 3)
    deleteEnd.collect() shouldBe "The quick brown fox jumps over the lazy "

  it should "handle insertions that cause rebalancing" in new ChunkedRopeSpecScope:
    // Create long chain by inserting at end repeatedly
    val rope = (1 to 100).foldLeft(Rope(""))((acc, i) => acc.insert(acc.weight, s"Line $i content "))

    // Should trigger rebalancing to maintain performance
    rope.isHeightBalanced shouldBe true
    rope.isWeightBalanced shouldBe true

    // Verify content is still correct after rebalancing
    val content = rope.collect()
    content should include("Line 1 content")
    content should include("Line 50 content")
    content should include("Line 100 content")

  it should "handle empty rope operations" in new RopeSpecScope:
    val empty = Rope("")

    empty.weight shouldBe 0
    empty.index(0) shouldBe None
    empty.search("test") shouldBe None
    empty.searchAll("test") shouldBe List.empty
    empty.splitAt(0) shouldBe Some((Leaf(""), Leaf("")))
    empty.insert(0, "hello").collect() shouldBe "hello"
    empty.deleteLeft(0, 5).collect() shouldBe ""
    empty.deleteRight(0, 5).collect() shouldBe ""

  it should "handle single character operations" in new RopeSpecScope:
    val single = Rope("a")

    single.weight shouldBe 1
    single.index(0) shouldBe Some('a')
    single.index(1) shouldBe None
    single.search("a") shouldBe Some(0)
    single.search("b") shouldBe None
    single.splitAt(0) shouldBe Some((Leaf(""), single))
    single.splitAt(1) shouldBe Some((single, Leaf("")))
    single.replace(0, 'b').collect() shouldBe "b"
    single.deleteLeft(1, 1).collect() shouldBe ""
    single.deleteRight(0, 1).collect() shouldBe ""

  it should "handle boundary index operations" in new RopeSpecScope:
    val rope = Rope("hello")

    // Test boundary indices
    rope.index(-1) shouldBe None
    rope.index(5) shouldBe None
    rope.index(100) shouldBe None

    rope.splitAt(-1) shouldBe None
    rope.splitAt(6) shouldBe None

    rope.replace(-1, 'x').collect() shouldBe "hello"
    rope.replace(5, 'x').collect() shouldBe "hello"

    rope.deleteLeft(-1, 2).collect() shouldBe "hello"
    rope.deleteRight(10, 2).collect() shouldBe "hello"

  it should "handle unicode and special characters" in new RopeSpecScope:
    val unicode = Rope("café 🚀 naïve")

    unicode.weight should be > 10 // Unicode chars take more bytes
    unicode.index(0) shouldBe Some('c')
    unicode.search("🚀") shouldBe defined
    unicode.searchAll("a") should have length 2
    unicode.replace(5, '⭐').collect() should include("⭐")

  it should "handle very long strings without stack overflow" in new ChunkedRopeSpecScope:
    // Test with string longer than leafChunkSize to force tree structure
    val longString = "a" * 1000
    val rope       = Rope(longString)

    rope.weight shouldBe 1000
    rope.index(500) shouldBe Some('a')
    rope.index(999) shouldBe Some('a')
    rope.search("a") shouldBe Some(0)
    rope.slice(100, 200).weight shouldBe 100

    // Test operations on long rope
    val inserted = rope.insert(500, "TEST")
    inserted.weight shouldBe 1004
    inserted.search("TEST") shouldBe Some(500)

    val deleted = rope.deleteRight(100, 100)
    deleted.weight shouldBe 900

  it should "maintain structural integrity after complex operations" in new ChunkedRopeSpecScope:
    // Perform a mix of operations using function composition
    val rope = Rope("initial")
      .insert(0, "prefix ")
      .insert(14, " suffix") // 14 = "prefix initial".length
      .replace(7, 'X')
      .deleteLeft(15, 3)
      .concat(Rope(" more"))

    // Should still be balanced and functional
    rope.isWeightBalanced shouldBe true
    rope.isHeightBalanced shouldBe true
    rope.collect() should not be empty

    // All operations should work
    rope.index(0) should be(defined)
    rope.search("prefix") should be(defined)
    rope.splitAt(5) should be(defined)

  it should "handle rapid alternating insertions and deletions" in new ChunkedRopeSpecScope:
    // Simulate editing session with insertions and deletions
    val finalRope = (1 to 100).foldLeft(Rope("base")) { (rope, i) =>
      val step1 = rope.insert(rope.weight / 2, s"$i")
      val step2 = step1.deleteRight(0, 1)
      val step3 = step2.insert(0, "x")
      val step4 = step3.deleteLeft(step3.weight, 1)

      // Verify consistency every few operations
      if i % 20 == 0 then
        step4.weight should be > 0
        step4.collect() should not be empty
        step4.isWeightBalanced shouldBe true

      step4
    }

    // Final verification
    finalRope.weight should be > 0
    finalRope.isWeightBalanced shouldBe true

  it should "handle degenerate cases" in new RopeSpecScope:
    // Test with empty search terms
    val rope = Rope("hello world")
    rope.search("") shouldBe None
    rope.searchAll("") shouldBe List.empty
    rope.replaceAll("", "x").collect() shouldBe "hello world"

    // Test with search terms longer than rope
    rope.search("this is way too long") shouldBe None
    rope.searchAll("very long search term") shouldBe List.empty

    // Test deletion with zero count
    rope.deleteLeft(5, 0).collect() shouldBe "hello world"
    rope.deleteRight(5, 0).collect() shouldBe "hello world"

  it should "preserve correctness after rebalancing operations" in new ChunkedRopeSpecScope:
    // Create unbalanced rope manually
    val left       = Rope("a" * 100)
    val right      = Rope("b")
    val unbalanced = left.concat(right)

    val original   = unbalanced.collect()
    val rebalanced = unbalanced.rebalance
    val rebuilt    = unbalanced.rebuild

    // All should have same content
    rebalanced.collect() shouldBe original
    rebuilt.collect() shouldBe original

    // Rebalanced versions should be more balanced
    rebalanced.isWeightBalanced shouldBe true
    rebuilt.isWeightBalanced shouldBe true

  it should "handle line operations correctly" in new ChunkedRopeSpecScope:
    val multiline = Rope("line1\nline2\nline3\n")

    multiline.lineCount shouldBe 4 // 3 lines + 1 for final newline
    multiline.getLine(0) shouldBe Some("line1")
    multiline.getLine(1) shouldBe Some("line2")
    multiline.getLine(2) shouldBe Some("line3")
    multiline.getLine(3) shouldBe Some("")
    multiline.getLine(4) shouldBe None
    multiline.getLine(-1) shouldBe None

  trait ChunkedRopeSpecScope:
    given balance: Balance =
      Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)
