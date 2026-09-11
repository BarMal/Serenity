package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for issue #1198's trie of the compound-flagged vocabulary: built once per dictionary load and walked
  * character-by-character to find every valid compound member starting at a position, replacing (for the free-form
  * COMPOUNDFLAG matcher) the hash-bucket-and-`startsWith` scan `HunspellCompoundMatcher` uses for COMPOUNDRULE.
  */
class HunspellCompoundTrieSpec extends AnyFlatSpec with Matchers:

  "CompoundTrie.build" should "return an empty trie for an empty vocabulary" in {
    val trie = CompoundTrie.build(Map.empty)

    CompoundTrie.wordsAt(trie, "foobar", 0) shouldBe Nil
  }

  "CompoundTrie.wordsAt" should "find every word starting at a position, including shared-prefix words of different lengths" in {
    val trie = CompoundTrie.build(
      Map(
        "foo"    -> Set("A"),
        "foobar" -> Set("B"),
        "bar"    -> Set("C")
      )
    )

    val matches = CompoundTrie.wordsAt(trie, "foobarbaz", 0).toMap

    matches shouldBe Map(3 -> Set("A"), 6 -> Set("B"))
  }

  it should "find matches starting mid-string, not only at position 0" in {
    val trie = CompoundTrie.build(Map("bar" -> Set("A"), "baz" -> Set("B")))

    CompoundTrie.wordsAt(trie, "foobarbaz", 3).toMap shouldBe Map(6 -> Set("A"))
    CompoundTrie.wordsAt(trie, "foobarbaz", 6).toMap shouldBe Map(9 -> Set("B"))
  }

  it should "return no matches when no trie word starts at the position" in {
    val trie = CompoundTrie.build(Map("foo" -> Set("A")))

    CompoundTrie.wordsAt(trie, "xyz", 0) shouldBe Nil
  }

  it should "merge flags when the same word is inserted more than once" in {
    val trie = CompoundTrie.build(Map("foo" -> Set("A", "B")))

    CompoundTrie.wordsAt(trie, "foo", 0).toMap shouldBe Map(3 -> Set("A", "B"))
  }

  it should "not match past the end of the text" in {
    val trie = CompoundTrie.build(Map("foobar" -> Set("A")))

    CompoundTrie.wordsAt(trie, "foo", 0) shouldBe Nil
  }

end HunspellCompoundTrieSpec
