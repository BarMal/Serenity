package com.serenity.spellcheck

import java.nio.file.Path
import java.util.Locale

import scala.util.control.NonFatal
import scala.util.matching.Regex

private[spellcheck] enum HunspellFlagMode:
  case Simple
  case Long
  case Num

/** `continuationFlags` (issue #1187) are the flags attached after '/' in a PFX/SFX rule's append field -- hunspell(5)'s
  * continuation classes, granted to the derived word for further affixation. Only CIRCUMFIX consumes them here (see
  * `HunspellFormat.expand`); every other rule ignores them, so a dictionary using continuation classes for anything
  * else sees no behavior change.
  */
final private[spellcheck] case class HunspellAffixRule(
    strip: String,
    append: String,
    condition: String,
    combineable: Boolean,
    continuationFlags: Set[String]
)

final private[spellcheck] case class HunspellAffixRules(
    flagMode: HunspellFlagMode,
    flagAliases: Map[String, Set[String]],
    prefixes: Map[String, List[HunspellAffixRule]],
    suffixes: Map[String, List[HunspellAffixRule]],
    replacements: Map[String, List[String]],
    needAffixFlag: Option[String],
    iconv: List[(String, String)],
    oconv: List[(String, String)],
    compoundRules: List[String],
    compoundMin: Int,
    circumfixFlag: Option[String],
    compoundFlag: Option[String],
    compoundBeginFlag: Option[String],
    compoundMiddleFlag: Option[String],
    compoundEndFlag: Option[String],
    compoundWordMax: Option[Int]
)

private[spellcheck] object HunspellAffixRules:

  val empty: HunspellAffixRules =
    HunspellAffixRules(
      HunspellFlagMode.Simple,
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      None,
      Nil,
      Nil,
      Nil,
      3,
      None,
      None,
      None,
      None,
      None,
      None
    )

/** One `.dic` entry: a dictionary root and the affix flags it carries. */
final private[spellcheck] case class HunspellEntry(word: String, flags: Set[String])

/** Reading of the Hunspell `.aff`/`.dic` file pair: parsing an affix file into `HunspellAffixRules`, parsing a
  * dictionary entry line, and expanding a root into every surface form its affix rules produce.
  *
  * These are deliberately one unit rather than a parser and a separate expander. The `.aff` file's FLAG mode and AF
  * alias table decide how a `.dic` entry's flag field is read at all (`parseEntry` and the PFX/SFX continuation fields
  * share `parseFlagList`), and the three directives with cross-file semantics -- NEEDAFFIX, CIRCUMFIX and the
  * continuation classes CIRCUMFIX chains through -- are parsed in one half and applied in the other. Splitting them
  * leaves neither half explicable on its own.
  *
  * `applyConversionTable` is the exception that is genuinely used elsewhere: ICONV/OCONV are Hunspell-format operations
  * applied at check time, not load time, so `SpellChecker` calls it directly.
  */
private[spellcheck] object HunspellFormat:

  /** Directives from the Hunspell affix format that this handwritten parser does not implement (morphological
    * generation, compound-validity filters, and similar). Rather than silently ignoring them -- which would mis-flag
    * words that rely on them -- their presence is surfaced as an explicit dictionary-load diagnostic. See the PR
    * description for why this project carries a partial parser instead of a dependency on Lucene's Hunspell
    * implementation.
    *
    * ICONV/OCONV and NEEDAFFIX (issue #1182); COMPOUNDRULE/COMPOUNDMIN and CIRCUMFIX (issue #1187); and
    * COMPOUNDFLAG/COMPOUNDBEGIN/COMPOUNDMIDDLE/COMPOUNDEND (accepting the older COMPOUNDLAST spelling as an alias) and
    * COMPOUNDWORDMAX -- Hunspell's free-form dictionary word-segmentation compounding, as opposed to COMPOUNDRULE's
    * explicit flag grammar (issue #1198) -- are implemented and intentionally absent from this set: see
    * `parseConversionTable`/`parseNeedAffixFlag`, `parseCompoundRules`/`parseCompoundMin`/`parseCircumfixFlag`, and
    * `parseCompoundFlag`/`parseCompoundBeginFlag`/`parseCompoundMiddleFlag`/`parseCompoundEndFlag`/
    * `parseCompoundWordMax`. The CHECKCOMPOUND family (plus SIMPLIFIEDTRIPLE), COMPOUNDSYLLABLE, SYLLABLENUM, and
    * ONLYINCOMPOUND remain unsupported and are deferred to a follow-up issue -- see the PR description for why.
    */
  private val UnsupportedAffixDirectives = Set(
    "COMPOUNDSYLLABLE",
    "SYLLABLENUM",
    "ONLYINCOMPOUND",
    "CHECKCOMPOUNDCASE",
    "CHECKCOMPOUNDDUP",
    "CHECKCOMPOUNDREP",
    "CHECKCOMPOUNDTRIPLE",
    "CHECKCOMPOUNDPATTERN",
    "SIMPLIFIEDTRIPLE",
    "PSEUDOROOT",
    "FORBIDDENWORD",
    "WARN",
    "FORBIDWARN",
    "LEMMA_PRESENT",
    "COMPLEXPREFIXES",
    "KEEPCASE",
    "FULLSTRIP",
    "BREAK",
    "MAP",
    "PHONE",
    "IGNORE"
  )

  def parseAffixRules(lines: List[String]): HunspellAffixRules =
    val flagMode           = parseFlagMode(lines)
    val flagAliases        = parseFlagAliases(lines, flagMode)
    val prefixRules        = parsePrefixOrSuffixRules(lines, "PFX", flagMode)
    val suffixRules        = parsePrefixOrSuffixRules(lines, "SFX", flagMode)
    val replacements       = parseReplacements(lines)
    val needAffixFlag      = parseNeedAffixFlag(lines)
    val iconv              = parseConversionTable(lines, "ICONV")
    val oconv              = parseConversionTable(lines, "OCONV")
    val compoundRules      = parseCompoundRules(lines)
    val compoundMin        = parseCompoundMin(lines)
    val circumfixFlag      = parseCircumfixFlag(lines)
    val compoundFlag       = parseSingleValueDirective(lines, "COMPOUNDFLAG")
    val compoundBeginFlag  = parseSingleValueDirective(lines, "COMPOUNDBEGIN")
    val compoundMiddleFlag = parseSingleValueDirective(lines, "COMPOUNDMIDDLE")
    val compoundEndFlag =
      parseSingleValueDirective(lines, "COMPOUNDEND").orElse(parseSingleValueDirective(lines, "COMPOUNDLAST"))
    val compoundWordMax = parseCompoundWordMax(lines)
    HunspellAffixRules(
      flagMode,
      flagAliases,
      prefixRules,
      suffixRules,
      replacements,
      needAffixFlag,
      iconv,
      oconv,
      compoundRules,
      compoundMin,
      circumfixFlag,
      compoundFlag,
      compoundBeginFlag,
      compoundMiddleFlag,
      compoundEndFlag,
      compoundWordMax
    )

  def unsupportedAffixDirectives(lines: List[String], affixPath: Path): List[String] =
    lines
      .flatMap(_.split("\\s+").toList.headOption)
      .filter(UnsupportedAffixDirectives.contains)
      .distinct
      .map(directive =>
        s"Unsupported Hunspell affix directive '$directive' in $affixPath is not applied " +
          "(words relying on it may be mis-flagged)"
      )

  /** `ICONV`/`OCONV` (hunspell(5)): an input/output character conversion table, one `directive from to` line per entry
    * after the `directive count` header. Real-world dictionaries (en_US.aff, fr_FR.aff, nl_NL.aff, and others) use it
    * for ligature and typographic-quote normalization, e.g. `ICONV ﬁ fi`. This implements only the common
    * literal-substring form seen in every real dictionary checked for issue #1182; the rarer `_` end-of-word anchor
    * form is read as an ordinary pattern rather than specially handled, so a rule using it is a safe no-op (its
    * pattern, containing a literal underscore, will not occur in real words) instead of matching the wrong position --
    * no real dictionary surveyed for this issue used it.
    */
  def applyConversionTable(word: String, table: List[(String, String)]): String =
    table.foldLeft(word) { case (converted, (from, to)) => converted.replace(from, to) }

  def parseEntry(line: String, affixRules: HunspellAffixRules): Option[HunspellEntry] =
    val withoutMorphology = line.takeWhile(char => !char.isWhitespace)
    val word              = withoutMorphology.takeWhile(_ != '/').trim
    Option(word)
      .filter(_.exists(_.isLetter))
      .map(word => HunspellEntry(word, parseEntryFlags(withoutMorphology, affixRules)))

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

  /** The `.aff` file's FLAG mode decides how every flag field in both files is tokenized -- a `.dic` entry's flags, a
    * PFX/SFX rule's continuation class, and an AF alias line all go through here.
    */
  def parseFlagList(flags: String, flagMode: HunspellFlagMode): Set[String] =
    flagMode match
      case HunspellFlagMode.Simple =>
        flags.toList.map(_.toString).toSet
      case HunspellFlagMode.Long =>
        flags.grouped(2).filter(_.length == 2).toSet
      case HunspellFlagMode.Num =>
        flags.split(",").map(_.trim).filter(_.nonEmpty).toSet

  private def parseEntryFlags(entry: String, affixRules: HunspellAffixRules): Set[String] =
    entry.dropWhile(_ != '/') match
      case "" => Set.empty
      case flagsWithSlash =>
        val flags = flagsWithSlash.drop(1)
        affixRules.flagAliases.getOrElse(flags, parseFlagList(flags, affixRules.flagMode))

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

  private def parseFlagMode(lines: List[String]): HunspellFlagMode =
    lines
      .collectFirst {
        case line if line.startsWith("FLAG ") =>
          line.stripPrefix("FLAG ").trim.toLowerCase(Locale.ROOT)
      }
      .flatMap {
        case "long" => Some(HunspellFlagMode.Long)
        case "num"  => Some(HunspellFlagMode.Num)
        case _      => Some(HunspellFlagMode.Simple)
      }
      .getOrElse(HunspellFlagMode.Simple)

  private def parsePrefixOrSuffixRules(
    lines: List[String],
    kind: String,
    flagMode: HunspellFlagMode
  ): Map[String, List[HunspellAffixRule]] =
    val combinability = parseAffixRuleCombinability(lines, kind)
    lines.foldLeft(Map.empty[String, List[HunspellAffixRule]]) { (rules, line) =>
      val columns = line.split("\\s+").toList
      columns match
        case ruleKind :: flag :: strip :: appendField :: condition :: _ if ruleKind == kind =>
          val (appendText, continuationFlags) = appendField.split("/", 2) match
            case Array(text, continuation) => text        -> parseFlagList(continuation, flagMode)
            case _                         => appendField -> Set.empty[String]
          val rule = HunspellAffixRule(
            strip = zeroAsEmpty(strip),
            append = zeroAsEmpty(appendText),
            condition = condition,
            combineable = combinability.getOrElse(flag, false),
            continuationFlags = continuationFlags
          )
          rules.updated(flag, rules.getOrElse(flag, Nil) :+ rule)
        case _ => rules
    }

  private def parseAffixRuleCombinability(lines: List[String], kind: String): Map[String, Boolean] =
    lines.foldLeft(Map.empty[String, Boolean]) { (combinability, line) =>
      line.split("\\s+").toList match
        case ruleKind :: flag :: crossProduct :: count :: Nil if ruleKind == kind && count.forall(_.isDigit) =>
          combinability.updated(flag, crossProduct.equalsIgnoreCase("Y"))
        case _ => combinability
    }

  private def parseFlagAliases(lines: List[String], flagMode: HunspellFlagMode): Map[String, Set[String]] =
    val (aliases, _) =
      lines.foldLeft((Map.empty[String, Set[String]], 0)) {
        case ((aliases, aliasIndex), line) =>
          line.split("\\s+").toList match
            case "AF" :: count :: Nil if count.forall(_.isDigit) =>
              aliases -> aliasIndex
            case "AF" :: flags :: _ =>
              val nextIndex = aliasIndex + 1
              aliases.updated(nextIndex.toString, parseFlagList(flags, flagMode)) -> nextIndex
            case _ =>
              aliases -> aliasIndex
      }
    aliases

  private def parseReplacements(lines: List[String]): Map[String, List[String]] =
    lines.foldLeft(Map.empty[String, List[String]]) { (replacements, line) =>
      line.split("\\s+").toList match
        case "REP" :: source :: replacement :: _ if !source.forall(_.isDigit) =>
          val key   = DictionaryWord.normalize(source)
          val value = DictionaryWord.normalize(replacement)
          replacements.updated(key, (replacements.getOrElse(key, Nil) :+ value).distinct)
        case _ =>
          replacements
    }

  /** `NEEDAFFIX <flag>` (hunspell(5)): marks a root as a "virtual stem", valid only when affixed -- the bare root must
    * be excluded from the accepted word set even though it appears in the .dic file, while its affixed forms remain
    * valid. `<flag>` is a single flag token in whatever representation the file's FLAG mode uses, matched directly
    * against an entry's parsed flag set.
    */
  private def parseNeedAffixFlag(lines: List[String]): Option[String] =
    lines
      .collectFirst {
        case line if line.startsWith("NEEDAFFIX ") => line.stripPrefix("NEEDAFFIX ").trim
      }
      .filter(_.nonEmpty)

  private def parseConversionTable(lines: List[String], directive: String): List[(String, String)] =
    lines.flatMap { line =>
      line.split("\\s+").toList match
        case candidate :: from :: to :: Nil if candidate == directive => Some(from -> to)
        case _                                                        => None
    }

  /** `CIRCUMFIX <flag>` (hunspell(5)): the flag that, when carried in a PFX/SFX rule's continuation class, marks that
    * rule as usable only paired with its circumfix counterpart -- see `expand`.
    */
  private def parseCircumfixFlag(lines: List[String]): Option[String] =
    lines
      .collectFirst {
        case line if line.startsWith("CIRCUMFIX ") => line.stripPrefix("CIRCUMFIX ").trim
      }
      .filter(_.nonEmpty)

  /** `COMPOUNDMIN <num>` (hunspell(5)): the minimum length, in characters, of a word usable as a compound member.
    * Defaults to 3, hunspell's own documented default, when absent.
    */
  private def parseCompoundMin(lines: List[String]): Int =
    lines
      .collectFirst {
        case line if line.startsWith("COMPOUNDMIN ") => line.stripPrefix("COMPOUNDMIN ").trim.toIntOption
      }
      .flatten
      .getOrElse(3)

  /** `COMPOUNDRULE` (hunspell(5)): one `COMPOUNDRULE count` header (skipped here -- its all-digit body distinguishes it
    * from a pattern line) followed by `count` `COMPOUNDRULE pattern` lines, each a small regex-like grammar over
    * compound flags. The raw pattern strings are kept as-is: `HunspellCompoundMatcher` owns that grammar and
    * re-tokenizes them on the pure matching path, which keeps `DictionaryContext` -- a public type -- free of the
    * matcher's internal token representation.
    */
  private def parseCompoundRules(lines: List[String]): List[String] =
    lines.flatMap { line =>
      line.split("\\s+").toList match
        case "COMPOUNDRULE" :: pattern :: Nil if !pattern.forall(_.isDigit) => Some(pattern)
        case _                                                              => None
    }

  /** `COMPOUNDFLAG`/`COMPOUNDBEGIN`/`COMPOUNDMIDDLE`/`COMPOUNDEND` (hunspell(5), issue #1198): each names a single flag
    * letter (in whatever representation the file's FLAG mode uses) that marks a dictionary word eligible as a free-form
    * compound member -- `COMPOUNDFLAG` anywhere in the compound, the other three only at the position their name says.
    * Verified against hunspell's own `tests/compoundflag.aff` (`COMPOUNDFLAG A`) and `tests/germancompounding.aff`
    * (`COMPOUNDBEGIN U` / `COMPOUNDMIDDLE V` / `COMPOUNDEND W`).
    */
  private def parseSingleValueDirective(lines: List[String], directive: String): Option[String] =
    lines
      .collectFirst { case line if line.startsWith(s"$directive ") => line.stripPrefix(s"$directive ").trim }
      .filter(_.nonEmpty)

  /** `COMPOUNDWORDMAX <num>` (hunspell(5)): the maximum number of dictionary words a free-form COMPOUNDFLAG compound
    * may segment into. Absent (`None`) means hunspell's documented default of unlimited -- naturally bounded in
    * practice by `word.length / compoundMin`, but `HunspellFreeCompoundMatcher` enforces it as an actual limit rather
    * than leaving it implicit, per issue #1198.
    */
  private def parseCompoundWordMax(lines: List[String]): Option[Int] =
    lines.collectFirst {
      case line if line.startsWith("COMPOUNDWORDMAX ") => line.stripPrefix("COMPOUNDWORDMAX ").trim.toIntOption
    }.flatten

  private def zeroAsEmpty(value: String): String =
    if value == "0" then "" else value
