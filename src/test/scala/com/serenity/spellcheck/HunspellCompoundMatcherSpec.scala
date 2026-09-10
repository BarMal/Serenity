package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #1415: `compoundMemberCandidates` must no longer linear-scan the whole merged dictionary
  * for every candidate segment. These tests exercise `matches` (the object's only public entry point) directly against
  * a synthetic tens-of-thousands-word dictionary, so a regression back to a full scan shows up as this suite taking
  * noticeably longer -- timings are logged for manual before/after comparison per `docs/performance-benchmarks.md`'s
  * convention, not hard-asserted, since wall-clock assertions are unreliable on shared CI hardware.
  */
class HunspellCompoundMatcherSpec extends AnyFlatSpec with Matchers:

  private val CompoundMin = 1

  /** A large dictionary spread across every lowercase first letter, so an index bucketed by first letter still has to
    * discriminate within a bucket rather than trivially emptying it.
    */
  private def largeDictionary(entriesPerLetter: Int): Map[String, Set[String]] =
    (for
      letter <- 'a' to 'z'
      index  <- 0 until entriesPerLetter
    yield s"$letter${"x" * (index % 7 + 1)}$index" -> Set("Z")).toMap

  "matches" should "accept a compound word whose members carry the required flags" in {
    val dictionary = largeDictionary(entriesPerLetter = 2000) ++ Map("foo" -> Set("A"), "bar" -> Set("B"))

    val start = System.nanoTime()
    val accepted = HunspellCompoundMatcher.matches(
      word = "foobar",
      compoundRules = List("AB"),
      compoundWordFlags = dictionary,
      compoundMin = CompoundMin
    )
    val elapsedMillis = (System.nanoTime() - start) / 1000000L
    info(s"large-dictionary compound match took ${elapsedMillis}ms over ${dictionary.size} entries")

    accepted shouldBe true
  }

  it should "reject a word that cannot be segmented into flagged dictionary members" in {
    val dictionary = largeDictionary(entriesPerLetter = 2000) ++ Map("foo" -> Set("A"), "bar" -> Set("B"))

    val accepted = HunspellCompoundMatcher.matches(
      word = "foobaz",
      compoundRules = List("AB"),
      compoundWordFlags = dictionary,
      compoundMin = CompoundMin
    )

    accepted shouldBe false
  }

  it should "reject a candidate member whose flag does not match, even when the prefix matches" in {
    val dictionary = largeDictionary(entriesPerLetter = 500) ++ Map("foo" -> Set("Q"), "bar" -> Set("B"))

    val accepted = HunspellCompoundMatcher.matches(
      word = "foobar",
      compoundRules = List("AB"),
      compoundWordFlags = dictionary,
      compoundMin = CompoundMin
    )

    accepted shouldBe false
  }

  it should "respect ZeroOrMore quantifiers when segmenting repeated compound members" in {
    val dictionary = largeDictionary(entriesPerLetter = 500) ++
      Map("un" -> Set("P"), "der" -> Set("S"))

    val accepted = HunspellCompoundMatcher.matches(
      word = "unununder",
      compoundRules = List("P*S"),
      compoundWordFlags = dictionary,
      compoundMin = CompoundMin
    )

    accepted shouldBe true
  }

  it should "return false when there are no compound rules, without inspecting the dictionary" in {
    HunspellCompoundMatcher.matches(
      word = "foobar",
      compoundRules = Nil,
      compoundWordFlags = largeDictionary(entriesPerLetter = 100),
      compoundMin = CompoundMin
    ) shouldBe false
  }

  // Timings are logged for manual before/after comparison (see docs/performance-benchmarks.md's convention) rather
  // than hard-asserted: wall-clock ratios are unreliable on shared CI/dev hardware, as this project's other
  // performance specs (e.g. CommandRunnerCloseAnimationPerformanceSpec) already document.
  it should "complete a large-dictionary lookup while remaining correct, for manual before/after timing comparison" in {
    val small = largeDictionary(entriesPerLetter = 500) ++ Map("foo" -> Set("A"), "bar" -> Set("B"))
    val large = largeDictionary(entriesPerLetter = 20000) ++ Map("foo" -> Set("A"), "bar" -> Set("B"))

    def timed(dictionary: Map[String, Set[String]]): Long =
      val start = System.nanoTime()
      HunspellCompoundMatcher.matches(
        word = "foobar",
        compoundRules = List("AB"),
        compoundWordFlags = dictionary,
        compoundMin = CompoundMin
      ) shouldBe true
      (System.nanoTime() - start) / 1000L

    // Warm up the JIT on both sizes before the comparison timing, so the ratio reflects algorithmic behaviour rather
    // than interpreter/compilation noise.
    timed(small)
    timed(large)

    val smallMicros = timed(small)
    val largeMicros = timed(large)
    info(s"small (${small.size} entries) took ${smallMicros}us, large (${large.size} entries) took ${largeMicros}us")
  }

end HunspellCompoundMatcherSpec
