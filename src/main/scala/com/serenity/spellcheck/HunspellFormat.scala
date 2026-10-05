package com.serenity.spellcheck

import java.util.Locale

private[spellcheck] enum HunspellFlagMode:
  case Simple
  case Long
  case Num

/** One `CHECKCOMPOUNDPATTERN` rule (hunspell(5), issue #1198): forbids compounding when the preceding compound member
  * ends with `endChars` (and, if `endFlag` is set, carries that flag) and the following member begins with `beginChars`
  * (and, if `beginFlag` is set, carries that flag). `replacement`, when present, is not a suppression of that forbid --
  * the literal concatenation stays forbidden -- but names an alternate, elided spelling of the boundary
  * (`endChars`+`beginChars` replaced by `replacement`) that IS accepted, verified against hunspell's own
  * `tests/checkcompoundpattern2.{aff,dic,good,wrong}` (`o b z`: "foobar" forbidden, "fozar" accepted) and
  * `tests/checkcompoundpattern3.{aff,dic,good,wrong}` (`o/X b/Y z`, flag-conditioned).
  */
final private[spellcheck] case class CheckCompoundPattern(
    endChars: String,
    endFlag: Option[String],
    beginChars: String,
    beginFlag: Option[String],
    replacement: Option[String]
)

/** The `CHECKCOMPOUND*`/`SIMPLIFIEDTRIPLE` boundary-validation family (hunspell(5), issue #1198): post-hoc filters
  * `HunspellFreeCompoundMatcher` applies to each adjacent pair of members in a segmentation it finds. `checkTriple`
  * forbids a literal triple-letter run at the boundary regardless of `simplifiedTriple`; `simplifiedTriple` instead
  * adds an alternate, elided-spelling segmentation (one repeated letter dropped) as also acceptable -- verified against
  * hunspell's own `tests/simplifiedtriple.{aff,dic,good,wrong}` ("glasssko" forbidden, "glassko" accepted).
  */
final private[spellcheck] case class CompoundCheckRules(
    checkCase: Boolean,
    checkDup: Boolean,
    checkRep: Boolean,
    checkTriple: Boolean,
    simplifiedTriple: Boolean,
    patterns: List[CheckCompoundPattern]
)

private[spellcheck] object CompoundCheckRules:
  val empty: CompoundCheckRules = CompoundCheckRules(false, false, false, false, false, Nil)

/** `continuationFlags` (issue #1187) are the flags attached after '/' in a PFX/SFX rule's append field -- hunspell(5)'s
  * continuation classes, granted to the derived word for further affixation. `AffixedWordList` reads them for twofold
  * suffixes, CIRCUMFIX and ONLYINCOMPOUND.
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
    compoundWordMax: Option[Int],
    compoundCheckRules: CompoundCheckRules,
    onlyInCompoundFlag: Option[String],
    breaksAtHyphens: Boolean = true
):

  /** Every flag a compound mechanism of this dictionary consults on a word, so only words carrying one need their flags
    * kept for compound matching.
    */
  def compoundRelevantFlags: Set[String] =
    HunspellCompoundMatcher.ruleFlags(compoundRules) ++
      List(compoundFlag, compoundBeginFlag, compoundMiddleFlag, compoundEndFlag).flatten ++
      compoundCheckRules.patterns.flatMap(pattern => pattern.endFlag.toList ++ pattern.beginFlag.toList)

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
      None,
      CompoundCheckRules.empty,
      None
    )

/** One `.dic` entry: a dictionary root and the affix flags it carries. */
final private[spellcheck] case class HunspellEntry(word: String, flags: Set[String])

/** Reading of the Hunspell `.aff`/`.dic` file pair: parsing an affix file into `HunspellAffixRules` and a dictionary
  * entry line into a `HunspellEntry`. The `.aff` file's FLAG mode and AF alias table decide how a `.dic` entry's flag
  * field is read at all (`parseEntry` and the PFX/SFX continuation fields share `parseFlagList`). Applying the rules to
  * recognise a word is `AffixedWordList`'s job.
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
    * ICONV/OCONV and NEEDAFFIX (issue #1182); COMPOUNDRULE/COMPOUNDMIN and CIRCUMFIX (issue #1187);
    * COMPOUNDFLAG/COMPOUNDBEGIN/COMPOUNDMIDDLE/COMPOUNDEND (accepting the older COMPOUNDLAST spelling as an alias) and
    * COMPOUNDWORDMAX -- Hunspell's free-form dictionary word-segmentation compounding, as opposed to COMPOUNDRULE's
    * explicit flag grammar (issue #1198, PR 1 of 2); and the CHECKCOMPOUND* boundary-validation family --
    * CHECKCOMPOUNDCASE/DUP/REP/TRIPLE, SIMPLIFIEDTRIPLE, CHECKCOMPOUNDPATTERN and ONLYINCOMPOUND (issue #1198, PR 2 of
    * 2) -- are implemented and intentionally absent from this set: see `parseConversionTable`/`parseNeedAffixFlag`,
    * `parseCompoundRules`/`parseCompoundMin`/`parseCircumfixFlag`,
    * `parseCompoundFlag`/`parseCompoundBeginFlag`/`parseCompoundMiddleFlag`/`parseCompoundEndFlag`/
    * `parseCompoundWordMax`, and `parseCompoundCheckRules`/`parseSingleValueDirective(lines, "ONLYINCOMPOUND")`.
    * COMPOUNDSYLLABLE and SYLLABLENUM remain unsupported: both are Hungarian-specific syllable-counting compounding
    * limits requiring a per-language vowel-counting heuristic that no real fixture surveyed for #1198 exercises in a
    * way this project can verify with confidence -- see the PR description for the full reasoning.
    *
    * MAP and PHONE only guide suggestions, which `SpellSuggester` generates without them, so they affect no word's
    * validity and are not reported. BREAK is honoured as far as prose needs: a word is checked part by part at hyphens
    * unless the dictionary says `BREAK 0` (hunspell's own default breaks at hyphens, and so do the dictionaries that
    * list hyphens and dashes explicitly).
    */
  private val UnsupportedAffixDirectives = Set(
    "COMPOUNDSYLLABLE",
    "SYLLABLENUM",
    "PSEUDOROOT",
    "FORBIDDENWORD",
    "WARN",
    "FORBIDWARN",
    "LEMMA_PRESENT",
    "COMPLEXPREFIXES",
    "KEEPCASE",
    "FULLSTRIP",
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
    val compoundWordMax    = parseCompoundWordMax(lines)
    val compoundCheckRules = parseCompoundCheckRules(lines)
    val onlyInCompoundFlag = parseSingleValueDirective(lines, "ONLYINCOMPOUND")
    val breaksAtHyphens    = !lines.exists(_.trim == "BREAK 0")
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
      compoundWordMax,
      compoundCheckRules,
      onlyInCompoundFlag,
      breaksAtHyphens
    )

  def unsupportedAffixDirectives(
    lines: List[String],
    affixSource: String,
    inertDirectives: Set[String] = Set.empty
  ): List[String] =
    lines
      .flatMap(_.split("\\s+").toList.headOption)
      .filter(directive => UnsupportedAffixDirectives.contains(directive) && !inertDirectives.contains(directive))
      .distinct
      .map(directive =>
        s"Unsupported Hunspell affix directive '$directive' in $affixSource is not applied " +
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
    * rule as usable only paired with its circumfix counterpart -- see `AffixedWordList`.
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

  /** The `CHECKCOMPOUND*`/`SIMPLIFIEDTRIPLE` boundary-validation family (hunspell(5), issue #1198):
    * `CHECKCOMPOUNDCASE`, `CHECKCOMPOUNDDUP`, `CHECKCOMPOUNDREP`, `CHECKCOMPOUNDTRIPLE` and `SIMPLIFIEDTRIPLE` are bare
    * directives (no argument on the line), and `CHECKCOMPOUNDPATTERN` is `parseCheckCompoundPatterns`'s own multi-line
    * block.
    */
  private def parseCompoundCheckRules(lines: List[String]): CompoundCheckRules =
    CompoundCheckRules(
      checkCase = parseBooleanDirective(lines, "CHECKCOMPOUNDCASE"),
      checkDup = parseBooleanDirective(lines, "CHECKCOMPOUNDDUP"),
      checkRep = parseBooleanDirective(lines, "CHECKCOMPOUNDREP"),
      checkTriple = parseBooleanDirective(lines, "CHECKCOMPOUNDTRIPLE"),
      simplifiedTriple = parseBooleanDirective(lines, "SIMPLIFIEDTRIPLE"),
      patterns = parseCheckCompoundPatterns(lines)
    )

  private def parseBooleanDirective(lines: List[String], directive: String): Boolean =
    lines.exists(_.trim == directive)

  /** `CHECKCOMPOUNDPATTERN` (hunspell(5), issue #1198): a `CHECKCOMPOUNDPATTERN count` header (skipped here, like
    * `parseCompoundRules`'s COMPOUNDRULE header) followed by `count` `CHECKCOMPOUNDPATTERN endchars[/flag]
    * beginchars[/flag] [replacement]` lines. Confirmed against hunspell's own `tests/checkcompoundpattern{,2,3}.aff`:
    * the bare form (`nny ny`, Hungarian), the flagless form with a replacement (`o b z`), and the flag-conditioned form
    * (`o/X b/Y z`); `0` as either `endchars` or `beginchars` means an empty string, matching `zeroAsEmpty`'s existing
    * convention for PFX/SFX strip/append fields -- e.g. `0/B /A` from real-world Dutch `.aff` usage.
    */
  private def parseCheckCompoundPatterns(lines: List[String]): List[CheckCompoundPattern] =
    lines.flatMap { line =>
      line.split("\\s+").toList match
        case "CHECKCOMPOUNDPATTERN" :: count :: Nil if count.forall(_.isDigit) =>
          None
        case "CHECKCOMPOUNDPATTERN" :: endToken :: beginToken :: rest if !endToken.forall(_.isDigit) =>
          val (endChars, endFlag)     = parseCheckCompoundPatternToken(endToken)
          val (beginChars, beginFlag) = parseCheckCompoundPatternToken(beginToken)
          Some(CheckCompoundPattern(endChars, endFlag, beginChars, beginFlag, rest.headOption))
        case _ =>
          None
    }

  private def parseCheckCompoundPatternToken(token: String): (String, Option[String]) =
    token.split("/", 2) match
      case Array(text, flag) => zeroAsEmpty(text)  -> Some(flag)
      case _                 => zeroAsEmpty(token) -> None

  private def zeroAsEmpty(value: String): String =
    if value == "0" then "" else value
