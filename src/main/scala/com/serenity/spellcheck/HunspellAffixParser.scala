package com.serenity.spellcheck

import java.nio.file.Path
import java.util.Locale

private[spellcheck] enum HunspellFlagMode:
  case Simple
  case Long
  case Num

/** `continuationFlags` (issue #1187) are the flags attached after '/' in a PFX/SFX rule's append field -- hunspell(5)'s
  * continuation classes, granted to the derived word for further affixation. Only CIRCUMFIX consumes them here (see
  * `HunspellWordExpander.expand`); every other rule ignores them exactly as before, so a dictionary using continuation
  * classes for anything else sees no behavior change.
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
    circumfixFlag: Option[String]
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
      None
    )

/** Parses a Hunspell `.aff` affix file's trimmed lines into `HunspellAffixRules`. */
private[spellcheck] object HunspellAffixParser:

  /** Directives from the Hunspell affix format that this handwritten parser does not implement (the free-form
    * COMPOUNDFLAG compounding mechanism, morphological generation, and similar). Rather than silently ignoring them --
    * which would mis-flag words that rely on them -- their presence is surfaced as an explicit dictionary-load
    * diagnostic. See the PR description for why this project carries a partial parser instead of a dependency on
    * Lucene's Hunspell implementation.
    *
    * ICONV/OCONV and NEEDAFFIX (issue #1182), and COMPOUNDRULE/COMPOUNDMIN and CIRCUMFIX (issue #1187), are implemented
    * and intentionally absent from this set -- see `parseConversionTable`/`parseNeedAffixFlag` and
    * `parseCompoundRules`/`parseCompoundMin`/`parseCircumfixFlag`. COMPOUNDFLAG/COMPOUNDBEGIN/COMPOUNDMIDDLE/
    * COMPOUNDLAST (Hunspell's free-form dictionary word-segmentation compounding, as opposed to COMPOUNDRULE's explicit
    * flag grammar) and the CHECKCOMPOUND family (plus SIMPLIFIEDTRIPLE) validation directives remain unsupported and
    * are deferred to a follow-up issue -- see the PR description for why.
    */
  private val UnsupportedAffixDirectives = Set(
    "COMPOUNDFLAG",
    "COMPOUNDBEGIN",
    "COMPOUNDMIDDLE",
    "COMPOUNDLAST",
    "COMPOUNDWORDMAX",
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

  def parse(lines: List[String]): HunspellAffixRules =
    val flagMode      = parseFlagMode(lines)
    val flagAliases   = parseFlagAliases(lines, flagMode)
    val prefixRules   = parsePrefixOrSuffixRules(lines, "PFX", flagMode)
    val suffixRules   = parsePrefixOrSuffixRules(lines, "SFX", flagMode)
    val replacements  = parseReplacements(lines)
    val needAffixFlag = parseNeedAffixFlag(lines)
    val iconv         = parseConversionTable(lines, "ICONV")
    val oconv         = parseConversionTable(lines, "OCONV")
    val compoundRules = parseCompoundRules(lines)
    val compoundMin   = parseCompoundMin(lines)
    val circumfixFlag = parseCircumfixFlag(lines)
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
      circumfixFlag
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

  def parseHunspellFlagList(flags: String, flagMode: HunspellFlagMode): Set[String] =
    flagMode match
      case HunspellFlagMode.Simple =>
        flags.toList.map(_.toString).toSet
      case HunspellFlagMode.Long =>
        flags.grouped(2).filter(_.length == 2).toSet
      case HunspellFlagMode.Num =>
        flags.split(",").map(_.trim).filter(_.nonEmpty).toSet

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
            case Array(text, continuation) => text        -> parseHunspellFlagList(continuation, flagMode)
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
              aliases.updated(nextIndex.toString, parseHunspellFlagList(flags, flagMode)) -> nextIndex
            case _ =>
              aliases -> aliasIndex
      }
    aliases

  private def parseReplacements(lines: List[String]): Map[String, List[String]] =
    lines.foldLeft(Map.empty[String, List[String]]) { (replacements, line) =>
      line.split("\\s+").toList match
        case "REP" :: source :: replacement :: _ if !source.forall(_.isDigit) =>
          val key   = SpellChecker.normalizeWord(source)
          val value = SpellChecker.normalizeWord(replacement)
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
    * rule as usable only paired with its circumfix counterpart -- see `HunspellWordExpander.expand`.
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
    * compound flags (`HunspellCompoundMatcher` tokenizes and matches the grammar itself; the raw pattern strings are
    * kept here and re-tokenized by the pure, filesystem-free matching path so this stays a plain `List[String]` that
    * `SpellChecker.DictionaryContext` -- a public type -- can carry without exposing `CompoundToken`).
    */
  private def parseCompoundRules(lines: List[String]): List[String] =
    lines.flatMap { line =>
      line.split("\\s+").toList match
        case "COMPOUNDRULE" :: pattern :: Nil if !pattern.forall(_.isDigit) => Some(pattern)
        case _                                                              => None
    }

  private def zeroAsEmpty(value: String): String =
    if value == "0" then "" else value
