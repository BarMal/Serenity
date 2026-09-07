package com.serenity.spellcheck

import scala.util.control.NonFatal
import scala.util.matching.Regex

final private[spellcheck] case class HunspellEntry(word: String, flags: Set[String])

/** Parses one `.dic` entry line and expands a dictionary root into every surface form its affix rules produce. */
private[spellcheck] object HunspellWordExpander:

  def parseEntry(line: String, affixRules: HunspellAffixRules): Option[HunspellEntry] =
    val withoutMorphology = line.takeWhile(char => !char.isWhitespace)
    val word              = withoutMorphology.takeWhile(_ != '/').trim
    Option(word)
      .filter(_.exists(_.isLetter))
      .map(word => HunspellEntry(word, parseHunspellFlags(withoutMorphology, affixRules)))

  def expand(entry: HunspellEntry, affixRules: HunspellAffixRules): Set[String] =
    val prefixRules = entry.flags.flatMap(flag => affixRules.prefixes.getOrElse(flag, Nil))
    val suffixRules = entry.flags.flatMap(flag => affixRules.suffixes.getOrElse(flag, Nil))

    def isCircumfix(rule: HunspellAffixRule): Boolean =
      affixRules.circumfixFlag.exists(rule.continuationFlags.contains)
    def circumfixPairValid(prefixRule: HunspellAffixRule, suffixRule: HunspellAffixRule): Boolean =
      isCircumfix(prefixRule) == isCircumfix(suffixRule)

    // CIRCUMFIX (#1187, hunspell(5)): an affix whose continuation class carries the CIRCUMFIX flag may never
    // surface on its own -- only paired with a counterpart that is itself CIRCUMFIX-flagged.
    val prefixes = prefixRules.filterNot(isCircumfix).flatMap(applyPrefix(entry.word, _))
    val suffixes = suffixRules.filterNot(isCircumfix).flatMap(applySuffix(entry.word, _))

    val combined =
      for
        prefixRule <- prefixRules if prefixRule.combineable
        suffixRule <- suffixRules if suffixRule.combineable && circumfixPairValid(prefixRule, suffixRule)
        suffixed   <- applySuffix(entry.word, suffixRule)
        combined   <- applyPrefix(suffixed, prefixRule)
      yield combined

    // CIRCUMFIX continuation chaining: the canonical Hungarian superlative ("legnagyobb") only reaches its prefix
    // via the suffix's continuation class -- the bare root never carries the prefix's own flag directly (see the
    // circumfix.aff fixture in hunspell's own test suite) -- so a CIRCUMFIX-flagged affix also looks for its
    // counterpart among the flags the *other* affix's continuation class grants, applying only when that
    // counterpart is itself CIRCUMFIX-flagged.
    val suffixThenPrefix: Set[String] =
      suffixRules.filter(isCircumfix).flatMap { suffixRule =>
        applySuffix(entry.word, suffixRule).toList.flatMap { suffixed =>
          suffixRule.continuationFlags
            .flatMap(flag => affixRules.prefixes.getOrElse(flag, Nil))
            .filter(isCircumfix)
            .flatMap(prefixRule => applyPrefix(suffixed, prefixRule))
        }
      }
    val prefixThenSuffix: Set[String] =
      prefixRules.filter(isCircumfix).flatMap { prefixRule =>
        applyPrefix(entry.word, prefixRule).toList.flatMap { prefixed =>
          prefixRule.continuationFlags
            .flatMap(flag => affixRules.suffixes.getOrElse(flag, Nil))
            .filter(isCircumfix)
            .flatMap(suffixRule => applySuffix(prefixed, suffixRule))
        }
      }

    // NEEDAFFIX (#1182): a root flagged with the configured NEEDAFFIX flag is a "virtual stem" -- valid only
    // affixed, per hunspell(5) -- so the bare word is dropped here while its affixed forms above are kept.
    val standalone = if affixRules.needAffixFlag.exists(entry.flags.contains) then Set.empty else Set(entry.word)
    standalone ++ prefixes ++ suffixes ++ combined ++ suffixThenPrefix ++ prefixThenSuffix

  private def parseHunspellFlags(entry: String, affixRules: HunspellAffixRules): Set[String] =
    entry.dropWhile(_ != '/') match
      case "" => Set.empty
      case flagsWithSlash =>
        val flags = flagsWithSlash.drop(1)
        affixRules.flagAliases.getOrElse(flags, HunspellAffixParser.parseHunspellFlagList(flags, affixRules.flagMode))

  private def applyPrefix(word: String, rule: HunspellAffixRule): Option[String] =
    Option.when(word.startsWith(rule.strip) && prefixConditionMatches(word, rule.condition)) {
      rule.append + word.drop(rule.strip.length)
    }

  private def applySuffix(word: String, rule: HunspellAffixRule): Option[String] =
    Option.when(word.endsWith(rule.strip) && suffixConditionMatches(word, rule.condition)) {
      word.dropRight(rule.strip.length) + rule.append
    }

  private def prefixConditionMatches(word: String, condition: String): Boolean =
    condition == "." || regexMatches(s"^(?:$condition).*", word)

  private def suffixConditionMatches(word: String, condition: String): Boolean =
    condition == "." || regexMatches(s".*(?:$condition)$$", word)

  private def regexMatches(pattern: String, word: String): Boolean =
    try Regex(pattern).pattern.matcher(word).matches
    catch case NonFatal(_) => false
