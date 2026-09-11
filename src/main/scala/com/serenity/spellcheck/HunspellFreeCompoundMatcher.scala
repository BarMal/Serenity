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
  *
  * PR 2 of 2 adds the `CHECKCOMPOUND*`/`SIMPLIFIEDTRIPLE`/`CHECKCOMPOUNDPATTERN` boundary-validation family
  * (`checkRules`) as a filter applied to every adjacent pair of members the DP considers. `CHECKCOMPOUNDPATTERN`'s
  * `replacement` field and `SIMPLIFIEDTRIPLE` are not mere filters, though -- both name an *alternate, elided spelling*
  * of a boundary that the literal concatenation forbids (verified against hunspell's own
  * `tests/checkcompoundpattern2.{aff,dic,good,wrong}` and `tests/simplifiedtriple.{aff,dic,good,wrong}`), so the DP
  * also generates reconstructed candidate members for those two directives alongside the plain trie walk.
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

  /** One compound member as the DP considers it: `text` is its canonical dictionary spelling -- which, for a
    * `CHECKCOMPOUNDPATTERN`/`SIMPLIFIEDTRIPLE` reconstruction, differs from the substring of the checked word this
    * member actually occupies -- and `flags` its dictionary flags. `skipPatternCheck`/`skipTripleCheck` mark a member
    * produced by exactly that reconstruction mechanism, so the boundary check the reconstruction exists to satisfy is
    * not re-applied to the very spelling that already satisfies it (see `violatesBoundary`).
    */
  final private case class Segment(
      text: String,
      flags: Set[String],
      firstChar: Char,
      lastChar: Char,
      skipPatternCheck: Boolean = false,
      skipTripleCheck: Boolean = false
  )

  /** True iff `word` can be fully segmented into two or more members of `trie`, each at least `compoundMin` characters
    * and eligible (per `compoundFlags`) for the position it would occupy, using no more than `compoundWordMax` members
    * when that bound is set, with every adjacent pair satisfying `checkRules`. `wordFlagsByText` (typically
    * `DictionaryContext.compoundWordFlags`), `standaloneWords` (`DictionaryContext.words`) and `replacements`
    * (`DictionaryContext.replacements`) back `CHECKCOMPOUNDPATTERN`/`SIMPLIFIEDTRIPLE` reconstruction and
    * `CHECKCOMPOUNDREP` respectively, and are unused (so safe to omit) when `checkRules` declares none of those.
    * `originalWord` -- same length as `word`, differing only in letter case -- backs `CHECKCOMPOUNDCASE`: dictionary
    * lookups always match against `word` (normalized, lower-case), but the case check itself must see the word as the
    * user actually typed it, so callers that normalize `word` before calling (as `SpellChecker.isAccepted` does) should
    * pass the pre-normalization text here; an empty string (the default -- a real dictionary word is never empty) or
    * any length mismatch falls back to `word` itself, so every caller that does not case-fold is unaffected.
    */
  def matches(
    word: String,
    trie: CompoundTrie,
    compoundFlags: CompoundFlags,
    compoundMin: Int,
    compoundWordMax: Option[Int],
    checkRules: CompoundCheckRules = CompoundCheckRules.empty,
    wordFlagsByText: Map[String, Set[String]] = Map.empty,
    standaloneWords: Set[String] = Set.empty,
    replacements: Map[String, List[String]] = Map.empty,
    originalWord: String = ""
  ): Boolean =
    val effectiveOriginal = if originalWord.length == word.length then originalWord else word
    compoundFlags != CompoundFlags.empty &&
    segment(
      word,
      trie,
      compoundFlags,
      compoundMin,
      compoundWordMax,
      checkRules,
      wordFlagsByText,
      standaloneWords,
      replacements,
      effectiveOriginal
    )

  private def segment(
    word: String,
    trie: CompoundTrie,
    compoundFlags: CompoundFlags,
    compoundMin: Int,
    compoundWordMax: Option[Int],
    checkRules: CompoundCheckRules,
    wordFlagsByText: Map[String, Set[String]],
    standaloneWords: Set[String],
    replacements: Map[String, List[String]],
    originalWord: String
  ): Boolean =
    // Local, contained mutation (a memo table for the (position, wordsUsed, previous member) DP state) -- never
    // leaves this function, matching this project's Mutation Policy for private, contained working state.
    val memo = scala.collection.mutable.Map.empty[(Int, Int, Option[String]), Boolean]

    def eligible(flags: Set[String], isFirst: Boolean, isLast: Boolean): Boolean =
      compoundFlags.general.exists(flags.contains) ||
        (isFirst && compoundFlags.begin.exists(flags.contains)) ||
        (isLast && compoundFlags.end.exists(flags.contains)) ||
        (!isFirst && !isLast && compoundFlags.middle.exists(flags.contains))

    // CHECKCOMPOUNDCASE reads the surface span [pos, endPos) of `originalWord` -- the word as typed, before
    // `SpellChecker.isAccepted` case-folded it for dictionary lookup -- rather than `text` (which, for a
    // CHECKCOMPOUNDPATTERN/SIMPLIFIEDTRIPLE reconstruction, is the reconstructed dictionary spelling, not what the
    // user actually typed at this position).
    def segmentAt(
      pos: Int,
      endPos: Int,
      text: String,
      flags: Set[String],
      skipPattern: Boolean = false,
      skipTriple: Boolean = false
    ): Segment =
      Segment(text, flags, originalWord.charAt(pos), originalWord.charAt(endPos - 1), skipPattern, skipTriple)

    def allIndicesOf(text: String, needle: String, from: Int): List[Int] =
      if needle.isEmpty || from > text.length - needle.length then Nil
      else
        text.indexOf(needle, from) match
          case -1  => Nil
          case pos => pos :: allIndicesOf(text, needle, pos + 1)

    // CHECKCOMPOUNDPATTERN reconstruction, first-word side: a member ending in `pattern.endChars` whose text was
    // elided from the checked word and replaced by `pattern.replacement` (hunspell's own tests/checkcompoundpattern2:
    // "foobar" forbidden, but "fozar" -- "fo" + endchars "o" reconstructing "foo" -- accepted).
    def patternWordsAsFirst(pos: Int): List[(Int, Segment)] =
      checkRules.patterns.flatMap { pattern =>
        pattern.replacement.toList.flatMap { replacement =>
          allIndicesOf(word, replacement, pos).flatMap { replacementPos =>
            val candidateText = word.substring(pos, replacementPos) + pattern.endChars
            wordFlagsByText
              .get(DictionaryWord.normalize(candidateText))
              .filter(flags => pattern.endFlag.forall(flags.contains))
              .map { flags =>
                val endPos = replacementPos + replacement.length
                (endPos, segmentAt(pos, endPos, candidateText, flags, skipPattern = true))
              }
          }
        }
      }

    // CHECKCOMPOUNDPATTERN reconstruction, second-word side: a member beginning with `pattern.beginChars`, present
    // only when the checked word actually spells `pattern.replacement` immediately before `pos`.
    def patternWordsAsSecond(pos: Int): List[(Int, Segment)] =
      checkRules.patterns.flatMap { pattern =>
        pattern.replacement.toList
          .filter(replacement =>
            pos >= replacement.length && word.substring(pos - replacement.length, pos) == replacement
          )
          .flatMap { _ =>
            (pos + 1 to word.length).toList.flatMap { endPos =>
              val candidateText = pattern.beginChars + word.substring(pos, endPos)
              wordFlagsByText
                .get(DictionaryWord.normalize(candidateText))
                .filter(flags => pattern.beginFlag.forall(flags.contains))
                .map(flags => (endPos, segmentAt(pos, endPos, candidateText, flags, skipPattern = true)))
            }
          }
      }

    // SIMPLIFIEDTRIPLE reconstruction: a member whose real first letter was elided because it duplicated the
    // previous member's trailing letter twice over (hunspell's own tests/simplifiedtriple: "glasssko" forbidden,
    // "glassko" -- "glass" + elided leading "s" reconstructing "sko" -- accepted).
    def simplifiedTripleWords(pos: Int, previous: Segment): List[(Int, Segment)] =
      if !(checkRules.checkTriple && checkRules.simplifiedTriple) then Nil
      else
        previous.text match
          case text if text.length >= 2 && text(text.length - 1) == text(text.length - 2) =>
            val elidedChar = text.last
            (pos + 1 to word.length).toList.flatMap { endPos =>
              val candidateText = elidedChar.toString + word.substring(pos, endPos)
              wordFlagsByText
                .get(DictionaryWord.normalize(candidateText))
                .map(flags => (endPos, segmentAt(pos, endPos, candidateText, flags, skipTriple = true)))
            }
          case _ => Nil

    def candidatesAt(pos: Int, previous: Option[Segment]): List[(Int, Segment)] =
      val plain = CompoundTrie.wordsAt(trie, word, pos).map {
        case (endPos, flags) =>
          (endPos, segmentAt(pos, endPos, word.substring(pos, endPos), flags))
      }
      val reconstructed =
        patternWordsAsFirst(pos) ++ patternWordsAsSecond(pos) ++ previous.toList.flatMap(simplifiedTripleWords(pos, _))
      plain ++ reconstructed

    def hasTripleRun(text: String): Boolean =
      text.sliding(3).exists(window => window.length == 3 && window(0) == window(1) && window(1) == window(2))

    def patternViolation(previous: Segment, next: Segment): Boolean =
      checkRules.patterns.exists { pattern =>
        previous.text.endsWith(pattern.endChars) && pattern.endFlag.forall(previous.flags.contains) &&
        next.text.startsWith(pattern.beginChars) && pattern.beginFlag.forall(next.flags.contains)
      }

    def repViolation(pairText: String): Boolean =
      replacements.exists {
        case (source, targets) =>
          allIndicesOf(pairText, source, 0).exists { sourcePos =>
            targets.exists { target =>
              val candidate = pairText.substring(0, sourcePos) + target + pairText.substring(sourcePos + source.length)
              standaloneWords.contains(DictionaryWord.normalize(candidate))
            }
          }
      }

    def violatesBoundary(previous: Segment, next: Segment): Boolean =
      (checkRules.checkCase && (previous.lastChar.isUpper || next.firstChar.isUpper)) ||
        (checkRules.checkDup && DictionaryWord.normalize(previous.text) == DictionaryWord.normalize(next.text)) ||
        (checkRules.checkTriple && !previous.skipTripleCheck && !next.skipTripleCheck &&
          hasTripleRun(previous.text.takeRight(2) + next.text.take(2))) ||
        (!previous.skipPatternCheck && !next.skipPatternCheck && patternViolation(previous, next)) ||
        (checkRules.checkRep && repViolation(previous.text + next.text))

    def canSegment(pos: Int, wordsUsed: Int, previous: Option[Segment]): Boolean =
      memo.getOrElseUpdate(
        (pos, wordsUsed, previous.map(_.text)),
        if pos == word.length then wordsUsed >= 2
        else if compoundWordMax.exists(wordsUsed >= _) then false
        else
          candidatesAt(pos, previous).exists {
            case (endPos, next) =>
              next.text.length >= compoundMin &&
              eligible(next.flags, isFirst = pos == 0, isLast = endPos == word.length) &&
              previous.forall(prev => !violatesBoundary(prev, next)) &&
              canSegment(endPos, wordsUsed + 1, Some(next))
          }
      )

    canSegment(0, 0, None)
