package com.serenity.spellcheck

private[spellcheck] enum CompoundQuantifier:
  case Exactly, ZeroOrOne, ZeroOrMore

final private[spellcheck] case class CompoundToken(flag: String, quantifier: CompoundQuantifier)

/** Hunspell COMPOUNDRULE matching (issue #1187): parses a dictionary's COMPOUNDRULE pattern strings into flag/
  * quantifier tokens and matches candidate compound words against them, segmenting the candidate into dictionary words
  * carrying the flags each token requires.
  *
  * Deliberately independent of the rest of the package: this is a self-contained grammar engine over plain strings,
  * running on the pure check-time path against an already-loaded `DictionaryContext`, and it names no Hunspell affix
  * type. Its whole interface is `matches`.
  */
private[spellcheck] object HunspellCompoundMatcher:

  def matches(
    word: String,
    compoundRules: List[String],
    compoundWordFlags: Map[String, Set[String]],
    compoundMin: Int
  ): Boolean =
    compoundRules.nonEmpty && {
      val index = buildCandidateIndex(compoundWordFlags)
      compoundRules.exists { pattern =>
        compoundMatches(tokenizeCompoundPattern(pattern), word, index, compoundMin)
      }
    }

  /** Every dictionary word bucketed by its first character, built once per [[matches]] call and threaded through the
    * recursive matchers below instead of the raw map -- so the (potentially many) candidate lookups a single `matches`
    * call performs each narrow their scan to words that could plausibly match, rather than re-walking the whole
    * dictionary per lookup (issue #1415).
    */
  private def buildCandidateIndex(compoundWordFlags: Map[String, Set[String]]): Map[Char, Vector[(String, Set[String])]] =
    compoundWordFlags.iterator
      .filter { case (word, _) => word.nonEmpty }
      .toVector
      .groupBy { case (word, _) => word.head }

  /** Parses one COMPOUNDRULE pattern into flag/quantifier tokens. A flag is either a single character (Simple flag
    * mode) or a parenthesized group (`(XX)`/`(1234)`, mandatory for Long/Num flag modes per hunspell(5): "With long and
    * numerical flag types, use only parenthesized flags"), optionally followed by `*` (0 or more) or `?` (0 or 1); a
    * flag with neither suffix matches exactly once.
    */
  private def tokenizeCompoundPattern(pattern: String): List[CompoundToken] =
    def loop(remaining: String, acc: List[CompoundToken]): List[CompoundToken] =
      if remaining.isEmpty then acc.reverse
      else
        val (flag, afterFlag) =
          if remaining.head == '(' then
            val closeIndex = remaining.indexOf(')')
            if closeIndex < 0 then (remaining.drop(1), "")
            else (remaining.substring(1, closeIndex), remaining.substring(closeIndex + 1))
          else (remaining.head.toString, remaining.tail)
        val (quantifier, rest) = afterFlag.headOption match
          case Some('*') => (CompoundQuantifier.ZeroOrMore, afterFlag.drop(1))
          case Some('?') => (CompoundQuantifier.ZeroOrOne, afterFlag.drop(1))
          case _         => (CompoundQuantifier.Exactly, afterFlag)
        loop(rest, CompoundToken(flag, quantifier) :: acc)
    loop(pattern, Nil)

  /** Every dictionary word (from `index`) that `remaining` starts with, is at least `compoundMin` characters long, and
    * carries `flag` -- paired with what remains of the candidate compound after removing it. Real dictionary words are
    * never empty, so this always strictly shortens `remaining`, which is what guarantees
    * `compoundMatches`/`compoundStarMatches` below terminate.
    *
    * Only the bucket for `remaining`'s first character is scanned -- every candidate word must start with that same
    * character to match, so words in other buckets can never satisfy `remaining.startsWith(word)`.
    */
  private def compoundMemberCandidates(
    flag: String,
    remaining: String,
    index: Map[Char, Vector[(String, Set[String])]],
    compoundMin: Int
  ): List[String] =
    if remaining.isEmpty then Nil
    else
      index.getOrElse(remaining.head, Vector.empty).iterator.collect {
        case (word, flags) if word.length >= compoundMin && flags.contains(flag) && remaining.startsWith(word) =>
          remaining.drop(word.length)
      }.toList

  /** Matches `remaining` against a COMPOUNDRULE pattern's tokens, recursively segmenting it into dictionary words
    * flagged for each token in turn; succeeds only when every token is satisfied and the entire candidate is consumed.
    * Mirrors hunspell(5)'s own description: pattern matching and word segmentation happen together, not as separate
    * phases, since which segmentation is valid depends on which flags the pattern still needs.
    */
  private def compoundMatches(
    tokens: List[CompoundToken],
    remaining: String,
    index: Map[Char, Vector[(String, Set[String])]],
    compoundMin: Int
  ): Boolean =
    tokens match
      case Nil => remaining.isEmpty
      case token :: rest =>
        token.quantifier match
          case CompoundQuantifier.Exactly =>
            compoundMemberCandidates(token.flag, remaining, index, compoundMin)
              .exists(next => compoundMatches(rest, next, index, compoundMin))
          case CompoundQuantifier.ZeroOrOne =>
            compoundMatches(rest, remaining, index, compoundMin) ||
            compoundMemberCandidates(token.flag, remaining, index, compoundMin)
              .exists(next => compoundMatches(rest, next, index, compoundMin))
          case CompoundQuantifier.ZeroOrMore =>
            compoundStarMatches(token, rest, remaining, index, compoundMin)

  private def compoundStarMatches(
    token: CompoundToken,
    rest: List[CompoundToken],
    remaining: String,
    index: Map[Char, Vector[(String, Set[String])]],
    compoundMin: Int
  ): Boolean =
    compoundMatches(rest, remaining, index, compoundMin) ||
      compoundMemberCandidates(token.flag, remaining, index, compoundMin)
        .exists(next => compoundStarMatches(token, rest, next, index, compoundMin))
