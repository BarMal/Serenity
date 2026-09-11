package com.serenity.spellcheck

/** Hunspell free-form COMPOUNDFLAG compounding (issue #1198): unlike `HunspellCompoundMatcher`'s COMPOUNDRULE grammar,
  * where the pattern tells you which flag to look for at each position, COMPOUNDFLAG places no constraint on order or
  * count -- any two or more compound-flagged dictionary words, each at least `compoundMin` characters, may be
  * concatenated in any order, subject only to `compoundWordMax` and COMPOUNDBEGIN/MIDDLE/END positional roles where
  * declared.
  *
  * Segmentation is a memoized DP over `(position, words used so far)`, walking a `CompoundTrie` once per position
  * rather than re-scanning the dictionary. This is the algorithm from #1198's investigation comment, chosen over
  * Aho-Corasick as appropriately scoped for single-word interactive spellcheck rather than multi-pattern corpus
  * scanning.
  */
private[spellcheck] object HunspellFreeCompoundMatcher:

  /** The flag letters (in whatever representation the dictionary's FLAG mode uses) that mark a word eligible as a
    * compound member: `general` (plain COMPOUNDFLAG, eligible anywhere) and the positional `begin`/`middle`/`end`
    * flags. All four are independent per hunspell(5) -- a word may carry the general flag, a positional flag, both, or
    * neither.
    */
  final case class CompoundFlags(
      general: Option[String],
      begin: Option[String],
      middle: Option[String],
      end: Option[String]
  )

  object CompoundFlags:
    val empty: CompoundFlags = CompoundFlags(None, None, None, None)

  /** True iff `word` can be fully segmented into two or more members of `trie`, each at least `compoundMin` characters
    * and eligible (per `compoundFlags`) for the position it would occupy, using no more than `compoundWordMax` members
    * when that bound is set.
    */
  def matches(
    word: String,
    trie: CompoundTrie,
    compoundFlags: CompoundFlags,
    compoundMin: Int,
    compoundWordMax: Option[Int]
  ): Boolean =
    compoundFlags != CompoundFlags.empty && segment(word, trie, compoundFlags, compoundMin, compoundWordMax)

  private def segment(
    word: String,
    trie: CompoundTrie,
    compoundFlags: CompoundFlags,
    compoundMin: Int,
    compoundWordMax: Option[Int]
  ): Boolean =
    // Local, contained mutation (a memo table for the (position, wordsUsed) DP state) -- never leaves this function,
    // matching this project's Mutation Policy for private, contained working state.
    val memo = scala.collection.mutable.Map.empty[(Int, Int), Boolean]

    def eligible(flags: Set[String], isFirst: Boolean, isLast: Boolean): Boolean =
      compoundFlags.general.exists(flags.contains) ||
        (isFirst && compoundFlags.begin.exists(flags.contains)) ||
        (isLast && compoundFlags.end.exists(flags.contains)) ||
        (!isFirst && !isLast && compoundFlags.middle.exists(flags.contains))

    def canSegment(pos: Int, wordsUsed: Int): Boolean =
      memo.getOrElseUpdate(
        (pos, wordsUsed),
        if pos == word.length then wordsUsed >= 2
        else if compoundWordMax.exists(wordsUsed >= _) then false
        else
          CompoundTrie.wordsAt(trie, word, pos).exists {
            case (endPos, flags) =>
              endPos - pos >= compoundMin &&
              eligible(flags, isFirst = pos == 0, isLast = endPos == word.length) &&
              canSegment(endPos, wordsUsed + 1)
          }
      )

    canSegment(0, 0)
