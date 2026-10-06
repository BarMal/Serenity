package com.serenity.spellcheck

/** Whether a word is correct under a loaded dictionary: a dictionary word or affixed form, or a compound of them. */
private[spellcheck] object WordAcceptance:

  /** `word` is looked up as `DictionaryWord.normalize` would key it, but compounding still sees it as typed, since
    * CHECKCOMPOUNDCASE depends on the case it was written in.
    */
  def accepts(word: String, dictionary: DictionaryContext): Boolean =
    val normalized = DictionaryWord.normalize(word)
    normalized.length < 3 || dictionary.knows(normalized) ||
    HunspellCompoundMatcher.matches(
      normalized,
      dictionary.compoundRules,
      dictionary.compoundCandidateIndex,
      dictionary.compoundMin
    ) ||
    // Free-form COMPOUNDFLAG compounding (#1198) is a second, independent mechanism a dictionary may declare
    // alongside COMPOUNDRULE (real Croatian/Persian .aff files do this) -- tried as a sibling so either mechanism
    // accepting the word is sufficient, and neither masks the other's rejection.
    HunspellFreeCompoundMatcher.matches(
      normalized,
      dictionary.compoundFlagTrie,
      HunspellFreeCompoundMatcher.CompoundFlags(
        dictionary.compoundFlag,
        dictionary.compoundBeginFlag,
        dictionary.compoundMiddleFlag,
        dictionary.compoundEndFlag
      ),
      dictionary.compoundMin,
      dictionary.compoundWordMax,
      // CHECKCOMPOUND*/SIMPLIFIEDTRIPLE/CHECKCOMPOUNDPATTERN (#1198, PR 2 of 2): a post-hoc filter over the
      // segmentation(s) the DP above finds, backed by the same word/flag/REP data the standalone lookup uses.
      dictionary.compoundCheckRules,
      dictionary.compoundWordFlags,
      dictionary.knows,
      dictionary.replacements,
      originalWord = word
    )
