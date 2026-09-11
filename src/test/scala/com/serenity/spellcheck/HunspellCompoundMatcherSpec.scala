package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #1415/#1445: `compoundMemberCandidates` must no longer linear-scan the whole merged
  * dictionary for every candidate segment, and `matches` must no longer rebuild `CompoundCandidateIndex` on every
  * call -- `DictionaryLoader.loadSnapshot` builds it once per dictionary load (mirroring `CompoundTrie` for the
  * free-form COMPOUNDFLAG path) and `matches`'s signature now only accepts the already-built index, so a caller has no
  * way to hand it the raw `compoundWordFlags` map for it to rebuild internally. These tests exercise `matches` (the
  * object's only public entry point) directly against a synthetic tens-of-thousands-word dictionary, so a regression
  * back to a full scan shows up as this suite taking noticeably longer -- timings are logged for manual before/after
  * comparison per `docs/performance-benchmarks.md`'s convention, not hard-asserted, since wall-clock assertions are
  * unreliable on shared CI hardware.
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
    val index       = CompoundCandidateIndex.build(dictionary)

    val start = System.nanoTime()
    val accepted = HunspellCompoundMatcher.matches(
      word = "foobar",
      compoundRules = List("AB"),
      candidateIndex = index,
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
      candidateIndex = CompoundCandidateIndex.build(dictionary),
      compoundMin = CompoundMin
    )

    accepted shouldBe false
  }

  it should "reject a candidate member whose flag does not match, even when the prefix matches" in {
    val dictionary = largeDictionary(entriesPerLetter = 500) ++ Map("foo" -> Set("Q"), "bar" -> Set("B"))

    val accepted = HunspellCompoundMatcher.matches(
      word = "foobar",
      compoundRules = List("AB"),
      candidateIndex = CompoundCandidateIndex.build(dictionary),
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
      candidateIndex = CompoundCandidateIndex.build(dictionary),
      compoundMin = CompoundMin
    )

    accepted shouldBe true
  }

  it should "return false when there are no compound rules, without inspecting the index" in {
    HunspellCompoundMatcher.matches(
      word = "foobar",
      compoundRules = Nil,
      candidateIndex = CompoundCandidateIndex.build(largeDictionary(entriesPerLetter = 100)),
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
      val index = CompoundCandidateIndex.build(dictionary)
      val start = System.nanoTime()
      HunspellCompoundMatcher.matches(
        word = "foobar",
        compoundRules = List("AB"),
        candidateIndex = index,
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

  // The perf regression #1445 fixes: with the index built once and reused, checking many words against the same
  // large dictionary costs roughly `wordCount` index builds' worth of work total (one, up front), not `wordCount`
  // full rebuilds -- exactly the shape `SpellChecker.isAccepted` exercises it in, once per unrecognized word in a
  // document. `matches`'s signature (it takes a `CompoundCandidateIndex`, never the raw `compoundWordFlags` map) makes
  // a per-call rebuild structurally impossible for any caller going through this entry point.
  it should "check many words against one large dictionary in time proportional to the word count, not word count times dictionary size" in {
    val dictionary = largeDictionary(entriesPerLetter = 20000) ++ Map("foo" -> Set("A"), "bar" -> Set("B"))
    val index      = CompoundCandidateIndex.build(dictionary)
    val words      = List.fill(500)("foobar")

    val start = System.nanoTime()
    val results = words.map { word =>
      HunspellCompoundMatcher.matches(word, List("AB"), index, CompoundMin)
    }
    val elapsedMillis = (System.nanoTime() - start) / 1000000L
    info(s"checked ${words.size} words against a ${dictionary.size}-entry dictionary in ${elapsedMillis}ms")

    results.distinct shouldBe List(true)
  }

end HunspellCompoundMatcherSpec
