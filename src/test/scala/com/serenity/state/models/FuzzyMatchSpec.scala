package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuzzyMatchSpec extends AnyFlatSpec with Matchers:

  private def ranksAbove(query: String, better: String, worse: String): Unit =
    val betterScore = FuzzyMatch.score(query, better).getOrElse(fail(s"'$query' should match '$better'"))
    val worseScore  = FuzzyMatch.score(query, worse).getOrElse(fail(s"'$query' should match '$worse'"))
    betterScore should be > worseScore

  "FuzzyMatch" should "match a query whose characters appear in order, however far apart" in {
    FuzzyMatch.score("smc", "src/main/core.txt") shouldBe defined
    FuzzyMatch.score("", "anything.txt") shouldBe defined
  }

  it should "exclude a path missing a query character, or holding them out of order" in {
    FuzzyMatch.score("xyz", "abc.txt") shouldBe None
    FuzzyMatch.score("ba", "ab.txt") shouldBe None
    FuzzyMatch.score("abcd", "abc") shouldBe None
  }

  it should "ignore case on both sides" in {
    FuzzyMatch.score("README", "docs/readme.md") shouldBe defined
    val upper = FuzzyMatch.score("README", "docs/README.md")
    FuzzyMatch.score("readme", "docs/README.md") shouldBe upper
  }

  it should "rank a match in the file name above the same match in a directory" in
    ranksAbove("foo", "bar/foo.txt", "foo/bar.txt")

  it should "rank consecutive characters above scattered ones" in
    ranksAbove("abc", "abcxx.txt", "axbxc.txt")

  it should "rank characters that start a word above ones inside a word" in {
    ranksAbove("fb", "foo_bar.txt", "fxxbxxx.txt")
    ranksAbove("fb", "src/fooBar.txt", "src/foobar.txt")
    ranksAbove("mc", "main-core.txt", "maxcore.txt")
  }

  it should "rank a shorter path above a longer one that matches the same way" in
    ranksAbove("app", "app.scala", "deep/nested/dir/app.scala")
