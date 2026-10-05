package com.serenity.spellcheck

import java.util.regex.Pattern

import scala.util.control.NonFatal

/** One PFX/SFX rule prepared for reverse application: `condition` is compiled once at load, and `None` stands for
  * hunspell's always-true `.` condition.
  */
final private[spellcheck] case class PreparedAffix(
    flag: String,
    strip: String,
    append: String,
    condition: Option[Pattern],
    combineable: Boolean,
    continuationFlags: Set[String]
):

  def conditionHolds(word: String): Boolean = condition.forall(_.matcher(word).matches)

/** A Hunspell `.dic` word list kept as stems and recognised by stripping affixes at lookup time (#1939), rather than
  * expanded into every surface form at load. Memory then grows with the number of stems instead of the number of forms
  * they generate, and a suffix whose continuation class grants a second suffix (hunspell's twofold suffixes) is
  * recognised without enumerating the product.
  *
  * `stems` maps each normalized stem to the flag set of every `.dic` entry spelled that way; homonyms stay separate so
  * one entry's NEEDAFFIX or ONLYINCOMPOUND flag cannot hide another's bare form.
  */
final private[spellcheck] class AffixedWordList(
    val stems: Map[String, List[Set[String]]],
    suffixesByAppend: Map[String, List[PreparedAffix]],
    prefixesByAppend: Map[String, List[PreparedAffix]],
    needAffixFlag: Option[String],
    circumfixFlag: Option[String],
    onlyInCompoundFlag: Option[String]
):

  private val longestSuffix = suffixesByAppend.keysIterator.map(_.length).maxOption.getOrElse(0)
  private val longestPrefix = prefixesByAppend.keysIterator.map(_.length).maxOption.getOrElse(0)

  def contains(word: String): Boolean =
    word.nonEmpty &&
      (isBareStem(word) || viaSuffix(word) || viaPrefix(word) || viaPrefixThenSuffix(word) || viaCircumfixChain(word))

  def isEmpty: Boolean = stems.isEmpty

  def size: Int = stems.size

  private def entries(stem: String): List[Set[String]] =
    stems.getOrElse(stem, Nil).filterNot(flags => onlyInCompoundFlag.exists(flags.contains))

  private def isBareStem(word: String): Boolean =
    entries(word).exists(flags => !needAffixFlag.exists(flags.contains))

  private def stemCarries(stem: String, flags: String*): Boolean =
    entries(stem).exists(entryFlags => flags.forall(entryFlags.contains))

  private def isCircumfix(affix: PreparedAffix): Boolean = circumfixFlag.exists(affix.continuationFlags.contains)

  private def isOnlyInCompound(affix: PreparedAffix): Boolean =
    onlyInCompoundFlag.exists(affix.continuationFlags.contains)

  private def standalone(affix: PreparedAffix): Boolean = !isCircumfix(affix) && !isOnlyInCompound(affix)

  /** Each suffix `word` could end with, paired with the base it leaves once the suffix's strip text is restored. */
  private def suffixStrips(word: String): Iterator[(PreparedAffix, String)] =
    (0 to math.min(longestSuffix, word.length)).iterator.flatMap { length =>
      suffixesByAppend.getOrElse(word.substring(word.length - length), Nil).iterator.collect {
        case affix if word.length - length + affix.strip.length > 0 =>
          affix -> (word.substring(0, word.length - length) + affix.strip)
      }
    }

  private def prefixStrips(word: String): Iterator[(PreparedAffix, String)] =
    (0 to math.min(longestPrefix, word.length)).iterator.flatMap { length =>
      prefixesByAppend.getOrElse(word.substring(0, length), Nil).iterator.collect {
        case affix if word.length - length + affix.strip.length > 0 =>
          affix -> (affix.strip + word.substring(length))
      }
    }

  private def viaSuffix(word: String): Boolean =
    suffixStrips(word).exists {
      case (outer, base) if outer.conditionHolds(base) && !isOnlyInCompound(outer) =>
        (standalone(outer) && stemCarries(base, outer.flag)) || viaInnerSuffix(base, outer)
      case _ => false
    }

  /** Hunspell's twofold suffixes: `base` is itself a suffixed form whose suffix grants `outer`'s flag. */
  private def viaInnerSuffix(base: String, outer: PreparedAffix): Boolean =
    standalone(outer) &&
      suffixStrips(base).exists { (inner, stem) =>
        inner.continuationFlags.contains(outer.flag) && standalone(inner) && inner.conditionHolds(stem) &&
        stemCarries(stem, inner.flag)
      }

  private def viaPrefix(word: String): Boolean =
    prefixStrips(word).exists { (prefix, base) =>
      standalone(prefix) && prefix.conditionHolds(base) && stemCarries(base, prefix.flag)
    }

  private def viaPrefixThenSuffix(word: String): Boolean =
    prefixStrips(word).exists { (prefix, suffixed) =>
      prefix.combineable && !isOnlyInCompound(prefix) && prefix.conditionHolds(suffixed) &&
      suffixStrips(suffixed).exists { (suffix, stem) =>
        suffix.combineable && !isOnlyInCompound(suffix) && isCircumfix(prefix) == isCircumfix(suffix) &&
        suffix.conditionHolds(stem) && stemCarries(stem, prefix.flag, suffix.flag)
      }
    }

  /** CIRCUMFIX pairs where only one half is on the stem and the other is granted by that half's continuation class. */
  private def viaCircumfixChain(word: String): Boolean =
    circumfixFlag.isDefined &&
      prefixStrips(word).exists { (prefix, suffixed) =>
        isCircumfix(prefix) && !isOnlyInCompound(prefix) && prefix.conditionHolds(suffixed) &&
        suffixStrips(suffixed).exists { (suffix, stem) =>
          isCircumfix(suffix) && !isOnlyInCompound(suffix) && suffix.conditionHolds(stem) &&
          ((suffix.continuationFlags.contains(prefix.flag) && stemCarries(stem, suffix.flag)) ||
            (prefix.continuationFlags.contains(suffix.flag) && stemCarries(stem, prefix.flag)))
        }
      }

private[spellcheck] object AffixedWordList:

  val empty: AffixedWordList = build(Iterator.empty, HunspellAffixRules.empty)

  def build(entries: Iterator[HunspellEntry], rules: HunspellAffixRules): AffixedWordList =
    val all = entries.toVector
    // A dictionary has a few hundred distinct flag sets across ~100k entries; sharing them is most of the saving.
    val sharedFlagSets = all.iterator.map(_.flags).toSet.map(flags => flags -> flags).toMap
    val stems = all
      .groupMap(entry => DictionaryWord.normalize(entry.word))(entry => sharedFlagSets(entry.flags))
      .view
      .mapValues(_.toList)
      .toMap
    new AffixedWordList(
      stems,
      prepare(rules.suffixes, suffix = true).groupBy(_.append),
      prepare(rules.prefixes, suffix = false).groupBy(_.append),
      rules.needAffixFlag,
      rules.circumfixFlag,
      rules.onlyInCompoundFlag
    )

  private def prepare(rules: Map[String, List[HunspellAffixRule]], suffix: Boolean): List[PreparedAffix] =
    rules.toList.flatMap { (flag, flagRules) =>
      flagRules.map { rule =>
        PreparedAffix(
          flag,
          DictionaryWord.normalize(rule.strip),
          DictionaryWord.normalize(rule.append),
          compileCondition(rule.condition, suffix),
          rule.combineable,
          rule.continuationFlags
        )
      }
    }

  // Stems are lower-cased, so conditions match case-insensitively. An unparsable condition never matches, as it never
  // did when conditions were compiled per expansion.
  private def compileCondition(condition: String, suffix: Boolean): Option[Pattern] =
    Option.when(condition != ".") {
      val anchored = if suffix then s".*(?:$condition)" else s"(?:$condition).*"
      try Pattern.compile(anchored, Pattern.DOTALL | Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE)
      catch case NonFatal(_) => Pattern.compile("(?!)")
    }
