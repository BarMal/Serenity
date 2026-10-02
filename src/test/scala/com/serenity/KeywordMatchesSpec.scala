package com.serenity

import com.serenity.document.KeywordMatches
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A keyword is found as a whole word: it must be bounded by the line's ends, whitespace or any other character that is
  * not a letter. No regular expressions are involved, so a term is never read as a pattern.
  */
class KeywordMatchesSpec extends AnyFlatSpec with Matchers:

  private def found(line: String, term: String): List[(Int, Int)] =
    KeywordMatches.occurrences(line, term).map(o => (o.start, o.end))

  "A keyword" should "be found as a whole word, ignoring case" in {
    found("Then LIZ left", "liz") shouldBe List((5, 8))
  }

  it should "not be found inside a longer word" in {
    found("the lizard and Elizabeth", "liz") shouldBe Nil
  }

  it should "be bounded by punctuation, digits and underscores as well as whitespace" in {
    found("Liz's coat, (Liz), Liz2 and _Liz_", "liz") shouldBe List((0, 3), (13, 16), (19, 22), (29, 32))
  }

  it should "be found at the start and the end of the line" in {
    found("Liz", "liz") shouldBe List((0, 3))
  }

  it should "be found each time it occurs, without overlapping" in {
    found("Liz Liz Liz", "liz") shouldBe List((0, 3), (4, 7), (8, 11))
  }

  it should "match a multi-word term across any gap of non-letters" in {
    found("Lady Liz, Lady  Liz and Lady, Liz but not Lady Eliza", "lady liz") shouldBe
      List((0, 8), (10, 19), (24, 33))
  }

  it should "not match a multi-word term on only its first word" in {
    found("Lady Eliza", "lady liz") shouldBe Nil
  }

  it should "match nothing when the term has no letters" in {
    found("1 2 3 -- !!", "  ") shouldBe Nil
    found("1 2 3", "2") shouldBe Nil
  }

  it should "treat characters that are special to regular expressions literally" in {
    found("Liz. (Liz) [Liz]", "li.") shouldBe Nil
    found("Liz. (Liz) [Liz]", "(liz)") shouldBe List((0, 3), (6, 9), (12, 15))
  }

  it should "match accented and non-Latin letters as letters" in {
    found("Zoë left; Zoëlla stayed", "zoë") shouldBe List((0, 3))
  }

  "A keyword's stored form" should "be its lower-cased words joined by single spaces" in {
    KeywordMatches.normalized("  Lady,   LIZ! ") shouldBe "lady liz"
    KeywordMatches.normalized("---") shouldBe ""
  }

  "The word at a column" should "be the run of letters touching it" in {
    KeywordMatches.wordAt("the Liz's coat", 5) shouldBe Some("Liz")
    KeywordMatches.wordAt("the Liz's coat", 4) shouldBe Some("Liz")
    KeywordMatches.wordAt("the Liz's coat", 7) shouldBe Some("Liz")
  }

  it should "be absent between words and on an empty line" in {
    KeywordMatches.wordAt("the  coat", 4) shouldBe None
    KeywordMatches.wordAt("", 0) shouldBe None
  }

  "The term at a column" should "be the longest listed term whose occurrence touches it" in {
    val terms = List("liz", "lady liz")

    KeywordMatches.termAt("Lady Liz left", 6, terms) shouldBe Some("lady liz")
    KeywordMatches.termAt("Lady Liz left", 1, terms) shouldBe Some("lady liz")
    KeywordMatches.termAt("Liz left", 2, terms) shouldBe Some("liz")
  }

  it should "be absent when no listed term touches the column" in {
    KeywordMatches.termAt("Lady Liz left", 11, List("liz")) shouldBe None
    KeywordMatches.termAt("Lady Liz left", 2, Nil) shouldBe None
  }
