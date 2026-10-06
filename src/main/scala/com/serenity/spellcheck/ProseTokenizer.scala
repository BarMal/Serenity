package com.serenity.spellcheck

import scala.util.matching.Regex

/** A candidate word on one line of text, `start` inclusive and `end` exclusive. */
final private[spellcheck] case class ProseWord(text: String, start: Int, end: Int)

/** One line of text and the candidate words in it. */
final private[spellcheck] case class ProseLine(index: Int, text: String, words: List[ProseWord])

/** Finds the words of a document that spelling applies to (#1808). Markdown syntax that merely sits next to prose --
  * code, link targets, tags, front matter -- is not prose, and checking it flags `https`, `github` and variable names.
  */
private[spellcheck] object ProseTokenizer:

  private enum Region:
    case Prose
    case FrontMatter
    case Fence(marker: Char)

  // A run of letters and digits, joined by apostrophes (straight, curly, modifier) and hyphens (ASCII, U+2010,
  // U+2011). Starting at the run's first letter keeps "19th-century" from yielding a "th-century" fragment.
  private val WordRun = """[\p{L}\p{M}\p{N}]+(?:['’ʼ\-‐‑][\p{L}\p{M}\p{N}]+)*""".r

  private val NotProse: Regex = List(
    """(`+).+?\1""",
    """</?[A-Za-z][^<>]*>""",
    """(?i)(?:\b[a-z][a-z0-9+.\-]*://|\bwww\.)[^\s<>]+""",
    """[\w.+\-]+@[\w\-]+(?:\.[\w\-]+)+""",
    """(?<=\])\([^)\s]*"""
  ).mkString("|").r

  private val Hyphens = Set('-', '‐', '‑')

  def lines(text: String): List[ProseLine] =
    val start: (Region, List[ProseLine]) = (Region.Prose, Nil)
    val (_, tokenized) = text.split("\n", -1).zipWithIndex.foldLeft(start) {
      case ((region, acc), (line, index)) =>
        val (next, words) = step(region, line, index)
        (next, ProseLine(index, line, words) :: acc)
    }
    tokenized.reverse.filter(_.words.nonEmpty)

  /** The hyphen-separated parts of `word`, or `word` itself when it has no hyphen. */
  def hyphenParts(word: ProseWord): List[ProseWord] =
    val breaks = word.text.indices.filter(index => Hyphens.contains(word.text.charAt(index)))
    val bounds = (-1 +: breaks) zip (breaks :+ word.text.length)
    bounds.toList.map((hyphen, next) =>
      ProseWord(word.text.substring(hyphen + 1, next), word.start + hyphen + 1, word.start + next)
    )

  private def step(region: Region, line: String, index: Int): (Region, List[ProseWord]) =
    val trimmed = line.trim
    region match
      case Region.FrontMatter =>
        (if trimmed == "---" || trimmed == "..." then Region.Prose else Region.FrontMatter, Nil)
      case Region.Fence(marker) =>
        (if trimmed.startsWith(marker.toString * 3) then Region.Prose else region, Nil)
      case Region.Prose if index == 0 && trimmed == "---" => (Region.FrontMatter, Nil)
      case Region.Prose =>
        fenceMarker(trimmed).fold((Region.Prose, wordsIn(line)))(marker => (Region.Fence(marker), Nil))

  private def fenceMarker(trimmed: String): Option[Char] =
    List('`', '~').find(marker => trimmed.startsWith(marker.toString * 3))

  private def wordsIn(line: String): List[ProseWord] =
    val skipped = NotProse.findAllMatchIn(line).map(found => found.start until found.end).toList
    WordRun
      .findAllMatchIn(line)
      .filterNot(found => skipped.exists(_.contains(found.start)))
      .filterNot(found => found.matched.exists(_.isDigit))
      .map(found => ProseWord(found.matched, found.start, found.end))
      .toList
