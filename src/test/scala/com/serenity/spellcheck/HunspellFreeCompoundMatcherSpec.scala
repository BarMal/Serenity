package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Hunspell free-form COMPOUNDFLAG matching (issue #1198): any two or more compound-flagged dictionary words, each at
  * least `compoundMin` characters, concatenated in any order and count -- unlike `HunspellCompoundMatcher`'s
  * COMPOUNDRULE grammar, there is no pattern to guide segmentation, so this is a memoized DP over `(position, words
  * used so far)` walking a `CompoundTrie`. Syntax cross-checked against hunspell's own `tests/compoundflag.{aff,dic,
  * good,wrong}` and `tests/germancompounding.aff` (COMPOUNDBEGIN/MIDDLE/END) fixtures.
  */
class HunspellFreeCompoundMatcherSpec extends AnyFlatSpec with Matchers:

  import HunspellFreeCompoundMatcher.CompoundFlags

  private val GeneralFlags = CompoundFlags(general = Some("A"), begin = None, middle = None, end = None)

  // Verified verbatim against hunspell's own tests/compoundflag.{aff,dic,good,wrong}: COMPOUNDMIN 3, COMPOUNDFLAG A,
  // dictionary foo/A bar/A xy/A yz/A. "foobar"/"barfoo"/"foobarfoo" are in compoundflag.good; "xyyz"/"fooxy"/"xyfoo"/
  // "fooxybar" are in compoundflag.wrong because "xy"/"yz" are below COMPOUNDMIN.
  private val CompoundFlagVocabulary = Map(
    "foo" -> Set("A"),
    "bar" -> Set("A"),
    "xy"  -> Set("A"),
    "yz"  -> Set("A")
  )

  private val CompoundFlagTrie = CompoundTrie.build(CompoundFlagVocabulary)

  "matches" should "accept a two-word free-form compound" in {
    HunspellFreeCompoundMatcher.matches(
      "foobar",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe true
  }

  it should "accept the same two words compounded in the opposite order (free-form, unlike COMPOUNDRULE)" in {
    HunspellFreeCompoundMatcher.matches(
      "barfoo",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe true
  }

  it should "accept a three-word free-form compound" in {
    HunspellFreeCompoundMatcher.matches(
      "foobarfoo",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe true
  }

  it should "reject a single dictionary word -- a compound needs two or more members" in {
    HunspellFreeCompoundMatcher.matches(
      "foo",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe false
  }

  it should "reject a compound member below COMPOUNDMIN even when a matching flag exists" in {
    // "xy" and "yz" both carry flag A but are 2 characters, below COMPOUNDMIN 3 -- compoundflag.wrong.
    HunspellFreeCompoundMatcher.matches(
      "xyyz",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe false
    HunspellFreeCompoundMatcher.matches(
      "fooxy",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe false
  }

  it should "reject a word that cannot be segmented into flagged members at all" in {
    HunspellFreeCompoundMatcher.matches(
      "foobaz",
      CompoundFlagTrie,
      GeneralFlags,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe false
  }

  it should "return false when no compound flags are configured, without walking the trie" in {
    HunspellFreeCompoundMatcher.matches(
      "foobar",
      CompoundFlagTrie,
      CompoundFlags.empty,
      compoundMin = 3,
      compoundWordMax = None
    ) shouldBe false
  }

  it should "enforce COMPOUNDWORDMAX, rejecting a compound with more segments than allowed" in {
    val trie  = CompoundTrie.build(Map("a" -> Set("A"), "b" -> Set("A"), "c" -> Set("A"), "d" -> Set("A")))
    val flags = CompoundFlags(general = Some("A"), begin = None, middle = None, end = None)

    HunspellFreeCompoundMatcher.matches("abcd", trie, flags, compoundMin = 1, compoundWordMax = Some(3)) shouldBe false
    HunspellFreeCompoundMatcher.matches("abc", trie, flags, compoundMin = 1, compoundWordMax = Some(3)) shouldBe true
  }

  // Real syntax verified against hunspell's own tests/germancompounding.aff: `COMPOUNDBEGIN U`, `COMPOUNDMIDDLE V`,
  // `COMPOUNDEND W` -- one flag letter per positional role.
  "matches with positional roles" should "require the first word to carry the begin flag" in {
    val trie  = CompoundTrie.build(Map("un" -> Set("U"), "der" -> Set("W")))
    val flags = CompoundFlags(general = None, begin = Some("U"), middle = None, end = Some("W"))

    HunspellFreeCompoundMatcher.matches("under", trie, flags, compoundMin = 2, compoundWordMax = None) shouldBe true
    // "der" only carries the end flag, so it cannot lead a compound.
    HunspellFreeCompoundMatcher.matches("derun", trie, flags, compoundMin = 2, compoundWordMax = None) shouldBe false
  }

  it should "require an interior word to carry the middle flag (or the general flag)" in {
    val trie  = CompoundTrie.build(Map("un" -> Set("U"), "der" -> Set("W"), "stand" -> Set("V")))
    val flags = CompoundFlags(general = None, begin = Some("U"), middle = Some("V"), end = Some("W"))

    HunspellFreeCompoundMatcher.matches(
      "understand",
      trie,
      flags,
      compoundMin = 2,
      compoundWordMax = None
    ) shouldBe false
    HunspellFreeCompoundMatcher.matches(
      "unstandder",
      trie,
      flags,
      compoundMin = 2,
      compoundWordMax = None
    ) shouldBe true
  }

  it should "accept a word carrying only the general flag in any position even when positional roles are declared" in {
    val trie  = CompoundTrie.build(Map("un" -> Set("U"), "over" -> Set("A"), "der" -> Set("W")))
    val flags = CompoundFlags(general = Some("A"), begin = Some("U"), middle = None, end = Some("W"))

    // "un" (begin-only) leads, "over" (general) trails -- the general flag makes "over" eligible in the last
    // position too, even though it declares no COMPOUNDEND flag of its own.
    HunspellFreeCompoundMatcher.matches("unover", trie, flags, compoundMin = 2, compoundWordMax = None) shouldBe true
    // "over" (general) leads, "der" (end-only) trails -- the general flag makes "over" eligible in the first
    // position too, even though it declares no COMPOUNDBEGIN flag of its own.
    HunspellFreeCompoundMatcher.matches("overder", trie, flags, compoundMin = 2, compoundWordMax = None) shouldBe true
  }

  it should "reject a valid-length word placed in a position its declared role does not permit" in {
    val trie  = CompoundTrie.build(Map("un" -> Set("U"), "der" -> Set("W")))
    val flags = CompoundFlags(general = None, begin = Some("U"), middle = None, end = Some("W"))

    // Both words exist and are long enough, but "der" (end-only) cannot lead and "un" (begin-only) cannot trail.
    HunspellFreeCompoundMatcher.matches("derun", trie, flags, compoundMin = 2, compoundWordMax = None) shouldBe false
  }

  // Stress/property-style test (per #1198's re-scoping): a highly-branching prefix set of many short compound-
  // flagged words sharing prefixes, confirming the memoized DP does not blow up combinatorially -- the concern
  // #1198's original issue text raised about free-form segmentation search.
  it should "complete in reasonable time over a highly-branching prefix set without exponential blowup" in {
    // Every 1-, 2- and 3-letter combination of 'a'..'e' is a compound-flagged word, so a long run of 'a's is
    // ambiguous at every position -- classic exponential-blowup shape for unmemoized segmentation search.
    val letters = "abcde"
    val words = (for
      length <- 1 to 3
      combo  <- lettersOfLength(letters, length)
    yield combo -> Set("A")).toMap
    val trie  = CompoundTrie.build(words)
    val flags = CompoundFlags(general = Some("A"), begin = None, middle = None, end = None)

    val candidate = "a" * 40
    val start     = System.nanoTime()
    val accepted =
      HunspellFreeCompoundMatcher.matches(candidate, trie, flags, compoundMin = 1, compoundWordMax = None)
    val elapsedMillis = (System.nanoTime() - start) / 1000000L
    info(s"branching-prefix free-form compound match over ${words.size} words took ${elapsedMillis}ms")

    accepted shouldBe true
    elapsedMillis should be < 2000L
  }

  private def lettersOfLength(alphabet: String, length: Int): List[String] =
    if length <= 0 then List("")
    else lettersOfLength(alphabet, length - 1).flatMap(prefix => alphabet.map(char => prefix + char))

  // CHECKCOMPOUND*/SIMPLIFIEDTRIPLE/CHECKCOMPOUNDPATTERN (issue #1198, PR 2 of 2): a post-hoc boundary-validation
  // filter over the segmentation(s) the DP above finds. Fixtures below are verified verbatim against hunspell's own
  // tests/checkcompound{case,dup,rep,triple,pattern,pattern2,pattern3}.{aff,dic,good,wrong} and
  // tests/simplifiedtriple.{aff,dic,good,wrong}, transcribed here into ASCII-only dictionaries where the upstream
  // fixture used accented Hungarian text, to keep this suite encoding-independent while preserving the exact
  // mechanism under test.

  private def matches(
    word: String,
    vocabulary: Map[String, Set[String]],
    checkRules: CompoundCheckRules,
    compoundMin: Int = 1,
    standaloneWords: Set[String] = Set.empty,
    replacements: Map[String, List[String]] = Map.empty
  ): Boolean =
    HunspellFreeCompoundMatcher.matches(
      word,
      CompoundTrie.build(vocabulary),
      GeneralFlags,
      compoundMin,
      compoundWordMax = None,
      checkRules,
      vocabulary,
      standaloneWords,
      replacements
    )

  // Verified against hunspell's own tests/checkcompoundcase.{aff,dic,wrong}: COMPOUNDFLAG A, dictionary
  // foo/A Bar/A BAZ/A. "fooBar"/"BAZBar"/"BAZfoo" are in checkcompoundcase.wrong -- an upper-case letter on either
  // side of a compound boundary is forbidden.
  "matches with CHECKCOMPOUNDCASE" should "forbid an upper-case letter at a compound boundary" in {
    val vocabulary = Map("foo" -> Set("A"), "Bar" -> Set("A"), "BAZ" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkCase = true)

    matches("fooBar", vocabulary, rules) shouldBe false
    matches("BAZBar", vocabulary, rules) shouldBe false
    matches("BAZfoo", vocabulary, rules) shouldBe false
  }

  it should "accept a compound with no case transition at the boundary" in {
    val vocabulary = Map("foo" -> Set("A"), "bar" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkCase = true)

    matches("foobar", vocabulary, rules) shouldBe true
  }

  // Verified against hunspell's own tests/checkcompounddup.{aff,dic,wrong}-style usage (real .aff files use this
  // directive to forbid e.g. "foofoo"): CHECKCOMPOUNDDUP forbids two adjacent identical compound members.
  "matches with CHECKCOMPOUNDDUP" should "forbid two adjacent identical compound members" in {
    val vocabulary = Map("foo" -> Set("A"), "bar" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkDup = true)

    matches("foofoo", vocabulary, rules) shouldBe false
    matches("foobar", vocabulary, rules) shouldBe true
  }

  it should "allow the same word twice when not adjacent" in {
    val vocabulary = Map("foo" -> Set("A"), "bar" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkDup = true)

    matches("foobarfoo", vocabulary, rules) shouldBe true
  }

  // Verified against the *mechanism* of hunspell's own tests/checkcompoundrep.{aff,dic,good,wrong} (Hungarian
  // "szerviz" example -- szer+viz forbidden via REP i->y because the REP-substituted compound spelling matches the
  // standalone dictionary word "szerviz" -- transcribed to ASCII here to keep this suite encoding-independent):
  // CHECKCOMPOUNDREP forbids a compound whenever substituting a REP table entry into the two-member boundary text
  // reconstructs a real standalone dictionary word.
  "matches with CHECKCOMPOUNDREP" should "forbid a compound whose REP substitution matches a standalone word" in {
    // szer/A + wiz/A concatenates to "szerwiz"; REP w->v turns it into the standalone word "szerviz".
    val vocabulary   = Map("szer" -> Set("A"), "wiz" -> Set("A"))
    val standalone   = Set("szerviz")
    val replacements = Map("w" -> List("v"))
    val rules        = CompoundCheckRules.empty.copy(checkRep = true)

    matches("szerwiz", vocabulary, rules, standaloneWords = standalone, replacements = replacements) shouldBe false
  }

  it should "accept a compound whose REP substitution does not match any standalone word" in {
    val vocabulary   = Map("viz" -> Set("A"), "szer" -> Set("A"))
    val standalone   = Set("szerviz")
    val replacements = Map("w" -> List("v"))
    val rules        = CompoundCheckRules.empty.copy(checkRep = true)

    matches("vizszer", vocabulary, rules, standaloneWords = standalone, replacements = replacements) shouldBe true
  }

  // Verified verbatim against hunspell's own tests/checkcompoundtriple.{aff,dic,good,wrong}: COMPOUNDFLAG A,
  // dictionary foo/A opera/A eel/A bare/A. "fooopera" ("oo"+"o") and "bareeel" ("e"+"ee") are in .wrong;
  // "operafoo"/"eelbare" are in .good.
  "matches with CHECKCOMPOUNDTRIPLE" should "forbid a triple-letter run spanning the compound boundary" in {
    val vocabulary = Map("foo" -> Set("A"), "opera" -> Set("A"), "eel" -> Set("A"), "bare" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkTriple = true)

    matches("fooopera", vocabulary, rules) shouldBe false
    matches("bareeel", vocabulary, rules) shouldBe false
    matches("operafoo", vocabulary, rules) shouldBe true
    matches("eelbare", vocabulary, rules) shouldBe true
  }

  // Verified verbatim against hunspell's own tests/simplifiedtriple.{aff,dic,good,wrong}: COMPOUNDFLAG A,
  // COMPOUNDMIN 2, dictionary glass/A sko/A. "glasssko" (unreduced, literal triple) is in .wrong; "glassko" (one
  // "s" elided) is in .good.
  "matches with CHECKCOMPOUNDTRIPLE and SIMPLIFIEDTRIPLE" should
    "still forbid the literal, unreduced triple-letter spelling" in {
      val vocabulary = Map("glass" -> Set("A"), "sko" -> Set("A"))
      val rules      = CompoundCheckRules.empty.copy(checkTriple = true, simplifiedTriple = true)

      matches("glasssko", vocabulary, rules, compoundMin = 2) shouldBe false
    }

  it should "accept the simplified (one letter elided) spelling of the same compound" in {
    val vocabulary = Map("glass" -> Set("A"), "sko" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkTriple = true, simplifiedTriple = true)

    matches("glassko", vocabulary, rules, compoundMin = 2) shouldBe true
  }

  it should "not accept the simplified spelling when SIMPLIFIEDTRIPLE is not also declared" in {
    val vocabulary = Map("glass" -> Set("A"), "sko" -> Set("A"))
    val rules      = CompoundCheckRules.empty.copy(checkTriple = true, simplifiedTriple = false)

    matches("glassko", vocabulary, rules, compoundMin = 2) shouldBe false
  }

  // Verified verbatim against hunspell's own tests/checkcompoundpattern2.{aff,dic,good,wrong}:
  // "CHECKCOMPOUNDPATTERN o b z", dictionary foo/A bar/A. "foobar" (literal "o"+"b" boundary) is in .wrong;
  // "fozar" (the "ob" boundary elided and replaced by "z") is in .good.
  "matches with CHECKCOMPOUNDPATTERN" should "forbid a literal boundary matching endchars/beginchars" in {
    val vocabulary = Map("foo" -> Set("A"), "bar" -> Set("A"))
    val pattern =
      CheckCompoundPattern(endChars = "o", endFlag = None, beginChars = "b", beginFlag = None, replacement = Some("z"))
    val rules = CompoundCheckRules.empty.copy(patterns = List(pattern))

    matches("foobar", vocabulary, rules) shouldBe false
  }

  it should "accept the replacement-elided spelling of the same forbidden boundary" in {
    val vocabulary = Map("foo" -> Set("A"), "bar" -> Set("A"))
    val pattern =
      CheckCompoundPattern(endChars = "o", endFlag = None, beginChars = "b", beginFlag = None, replacement = Some("z"))
    val rules = CompoundCheckRules.empty.copy(patterns = List(pattern))

    matches("fozar", vocabulary, rules) shouldBe true
  }

  // Verified verbatim against hunspell's own tests/checkcompoundpattern3.{aff,dic,good,wrong}:
  // "CHECKCOMPOUNDPATTERN o/X b/Y z", dictionary foo/A boo/AX bar/A ban/AY. "booban" (literal) is in .wrong;
  // "bozan" (elided/replaced) is in .good; "boobar" (endchars matches but bar lacks the Y flag, so the pattern does
  // not apply at all) is in .good.
  "matches with flag-conditioned CHECKCOMPOUNDPATTERN" should "forbid only when both flags are present" in {
    val vocabulary =
      Map("foo" -> Set("A"), "boo" -> Set("A", "X"), "bar" -> Set("A"), "ban" -> Set("A", "Y"))
    val pattern = CheckCompoundPattern(
      endChars = "o",
      endFlag = Some("X"),
      beginChars = "b",
      beginFlag = Some("Y"),
      replacement = Some("z")
    )
    val rules = CompoundCheckRules.empty.copy(patterns = List(pattern))

    matches("booban", vocabulary, rules) shouldBe false
    matches("bozan", vocabulary, rules) shouldBe true
    matches("boobar", vocabulary, rules) shouldBe true
  }

end HunspellFreeCompoundMatcherSpec
