package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkFixturesSpec extends AnyFlatSpec with Matchers:

  "randomLetters" should "produce the same sequence for the same seed" in {
    BenchmarkFixtures.randomLetters(42L).take(500).toList shouldBe BenchmarkFixtures.randomLetters(42L).take(500).toList
  }

  it should "produce a different sequence for a different seed" in {
    BenchmarkFixtures.randomLetters(42L).take(500).toList should not be BenchmarkFixtures
      .randomLetters(43L)
      .take(500)
      .toList
  }

  it should "emit only lowercase letters and spaces" in {
    all(BenchmarkFixtures.randomLetters(42L).take(5_000).toList) should (be(' ') or (be >= 'a' and be <= 'z'))
  }

  it should "make about one character in six a space" in {
    val sample = BenchmarkFixtures.randomLetters(42L).take(60_000).toList
    sample.count(_ == ' ').toDouble / sample.size shouldBe (1.0 / 6.0 +- 0.01)
  }

  it should "not repeat a short cycle like the alphabet" in {
    BenchmarkFixtures.randomLetters(42L).take(26 * 20).grouped(26).toSet.size should be > 1
  }

  "longParagraph" should "have exactly the requested length" in
    List(1, 100, 4_000, 4_001).foreach(chars => BenchmarkFixtures.longParagraph(chars).length shouldBe chars)

  it should "be a single paragraph with no newlines" in {
    BenchmarkFixtures.longParagraph(4_000) should not include "\n"
  }

  it should "be lorem prose of lowercase words separated by single spaces" in {
    val text = BenchmarkFixtures.longParagraph(4_000)
    text.trim shouldBe text
    text should not include "  "
    text should startWith("lorem ipsum")
  }

  it should "be deterministic" in {
    BenchmarkFixtures.longParagraph(4_000) shouldBe BenchmarkFixtures.longParagraph(4_000)
  }

  it should "be empty for zero characters" in {
    BenchmarkFixtures.longParagraph(0) shouldBe ""
  }

  it should "never end on a space at any length" in
    (1 to 200).foreach(chars => BenchmarkFixtures.longParagraph(chars).last should not be ' ')
