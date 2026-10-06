package com.serenity.spellcheck

import java.util.Locale

/** Ranked corrections for one misspelled word (#1939), computed only when somebody asks for them -- a context menu or
  * the palette -- never by the background analysis pass, since searching near spellings costs milliseconds per word.
  *
  * Candidates come from four sources, cheapest and most trustworthy first: the `.aff` REP table applied anywhere in the
  * word, every single edit (Damerau-Levenshtein 1) that is a word, the dictionary's own stems within two edits, and
  * stems within one edit of the word with a suffix taken off, put back on once corrected (so "organisatons" reaches
  * "organisations" through "organisation"), plus a split into two words ("alot" to "a lot"). A candidate is only
  * offered if the dictionary accepts it, and is ranked by its edit distance from the typo.
  */
object SpellSuggester:

  val DefaultLimit: Int = 8

  private val FallbackAlphabet = "abcdefghijklmnopqrstuvwxyz'"

  private val ReplacementCost   = 5
  private val TranspositionCost = 8
  private val DoubledLetterCost = 9
  private val SplitCost         = 9
  private val EditCost          = 10
  private val FirstLetterCost   = 4

  private val MinAffixedBaseLength = 4
  private val MaxSuffixSplits      = 6
  private val MinSplitPartLength   = 3

  def suggest(word: String, dictionary: DictionaryContext, limit: Int = DefaultLimit): List[String] =
    val target = DictionaryWord.normalize(
      HunspellFormat.applyConversionTable(word, dictionary.iconv).replace('’', '\'').replace('ʼ', '\'')
    )
    if target.length < 2 then Nil
    else
      val found = candidates(target, dictionary).filter((candidate, _) => candidate != target)
      val best  = found.groupMapReduce((candidate, _) => candidate)((_, cost) => cost)(math.min)
      best.toList
        .sortBy((candidate, cost) => (cost + firstLetterPenalty(target, candidate), candidate.length, candidate))
        .iterator
        .map((candidate, _) => candidate)
        .filter(suggestible(_, dictionary))
        .map(candidate => HunspellFormat.applyConversionTable(matchCase(word, candidate), dictionary.oconv))
        .take(limit)
        .toList

  private def candidates(target: String, dictionary: DictionaryContext): List[(String, Int)] =
    val accepted = (candidate: String) => WordAcceptance.accepts(candidate, dictionary)
    val replaced = replacementEdits(target, dictionary).filter(accepted).map(_ -> ReplacementCost)
    val edited   = singleEdits(target, alphabet(dictionary)).filter((candidate, _) => accepted(candidate))
    val split    = splits(target, dictionary).map(_ -> SplitCost)
    val near =
      if replaced.size + edited.size >= DefaultLimit then Nil
      else nearStems(target, dictionary) ++ nearAffixed(target, dictionary)
    replaced ++ edited ++ split ++ near

  private def alphabet(dictionary: DictionaryContext): String =
    val declared = dictionary.tryCharacters.toLowerCase(Locale.ROOT).filter(char => char.isLetter || char == '\'')
    (if declared.isEmpty then FallbackAlphabet else declared).distinct

  private def replacementEdits(target: String, dictionary: DictionaryContext): List[String] =
    dictionary.replacements.toList.flatMap { (from, replacements) =>
      Option.when(from.nonEmpty)(from).toList.flatMap { pattern =>
        target.indices.filter(target.startsWith(pattern, _)).toList.flatMap { at =>
          replacements
            .map(replacement => target.take(at) + replacement.replace('_', ' ') + target.drop(at + pattern.length))
        }
      }
    }

  /** Every word one edit from `target`, each with the cost of that kind of slip: a swap of two neighbouring letters is
    * the commonest and a doubled or missing double letter next, so those rank ahead of other insertions, deletions and
    * substitutions.
    */
  private def singleEdits(target: String, letters: String): List[(String, Int)] =
    val halves = (0 to target.length).map(at => target.take(at) -> target.drop(at)).toList
    val deletes = halves.collect {
      case (left, right) if right.nonEmpty =>
        left + right.drop(1) -> (if left.lastOption.contains(right.charAt(0)) then DoubledLetterCost else EditCost)
    }
    val transposes = halves.collect {
      case (left, right) if right.length > 1 && right.charAt(0) != right.charAt(1) =>
        left + right.charAt(1) + right.charAt(0) + right.drop(2) -> TranspositionCost
    }
    val replaces = halves.flatMap { (left, right) =>
      if right.isEmpty then Nil else letters.map(letter => left + letter + right.drop(1) -> EditCost)
    }
    val inserts = halves.flatMap { (left, right) =>
      letters.map { letter =>
        left + letter + right ->
          (if left.lastOption.contains(letter) || right.headOption.contains(letter) then DoubledLetterCost
           else EditCost)
      }
    }
    (deletes ++ transposes ++ replaces ++ inserts)
      .groupMapReduce((candidate, _) => candidate)((_, cost) => cost)(
        math.min
      )
      .toList

  /** Two words the typo is a run-together of ("alot"): a lone "a" or "i" and a word, or two words of three letters or
    * more.
    */
  private def splits(target: String, dictionary: DictionaryContext): List[String] =
    (1 until target.length).toList.collect {
      case at
          if wordPart(target.take(at), target.drop(at), dictionary) &&
            wordPart(target.drop(at), target.take(at), dictionary) =>
        target.take(at) + " " + target.drop(at)
    }

  private def wordPart(part: String, other: String, dictionary: DictionaryContext): Boolean =
    dictionary.knows(part) && (part.length >= MinSplitPartLength || (part == "a" && other.length >= MinSplitPartLength))

  private def nearStems(target: String, dictionary: DictionaryContext): List[(String, Int)] =
    dictionary.stems.flatMap(_.nearStems(target, 2)).map((stem, distance) => stem -> distance * EditCost)

  /** Corrections of the part of `target` before a suffix, with the suffix put back where the corrected stem takes it:
    * the stem scan above cannot reach "judgement" from "jugdment", whose stem "jugd" is far from "judge" only because
    * the suffix is part of what it measures.
    */
  private def nearAffixed(target: String, dictionary: DictionaryContext): List[(String, Int)] =
    dictionary.stems.flatMap { stems =>
      stems
        .suffixSplits(target)
        .filter((_, base) => base.length >= MinAffixedBaseLength)
        .sortBy((affix, _) => -affix.append.length)
        .distinctBy((affix, base) => (base, affix.flag))
        .take(MaxSuffixSplits)
        .flatMap { (affix, base) =>
          stems
            .nearStems(base, 2)
            .flatMap((stem, distance) => stems.withSuffix(stem, affix).map(_ -> distance * EditCost))
        }
    }

  private def suggestible(candidate: String, dictionary: DictionaryContext): Boolean =
    dictionary.stems.forall(_.isSuggestible(candidate))

  private def firstLetterPenalty(target: String, candidate: String): Int =
    if target.headOption == candidate.headOption then 0 else FirstLetterCost

  private def matchCase(original: String, suggestion: String): String =
    if original.length > 1 && original.exists(_.isLetter) && original.forall(char => !char.isLower) then
      suggestion.toUpperCase(Locale.ROOT)
    else if original.headOption.exists(_.isUpper) then suggestion.take(1).toUpperCase(Locale.ROOT) + suggestion.drop(1)
    else suggestion
