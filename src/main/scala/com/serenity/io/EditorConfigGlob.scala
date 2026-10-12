package com.serenity.io

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.util.Try

/** A section name of an `.editorconfig` file, matched against a path relative to the directory holding the file. `*`
  * and `?` stay within a path segment, `**` crosses them, `[seq]`, `[!seq]`, `{a,b}` and `{n1..n2}` select, and `\`
  * escapes. A name with no `/` (other than inside a character class) matches at any depth, and one with a `/` is
  * anchored to the file's directory.
  */
final case class EditorConfigGlob(regex: String, ranges: Vector[EditorConfigGlob.IntegerRange]):

  // A name that does not form a valid expression, such as the reversed range `[z-a]`, matches nothing.
  private lazy val compiled: Option[Pattern] = Try(Pattern.compile(regex)).toOption

  def matches(relativePath: String): Boolean =
    compiled.exists { pattern =>
      val matcher = pattern.matcher(relativePath)
      matcher.matches() && ranges.forall(range => range.admits(Option(matcher.group(range.group))))
    }

object EditorConfigGlob:

  /** A `{min..max}` in a name: the regex captures its digits as `group`, and the number must lie within the bounds. */
  final case class IntegerRange(group: Int, min: Long, max: Long):

    def admits(digits: Option[String]): Boolean =
      digits.forall(_.toLongOption.exists(number => number >= min && number <= max))

  def compile(sectionName: String): EditorConfigGlob =
    val translated = translate(sectionName.stripPrefix("/"), groupsBefore = 0)
    val anchored   = sectionName.startsWith("/") || translated.hasSeparator
    EditorConfigGlob((if anchored then "" else "(?:.*/)?") + translated.regex, translated.ranges)

  final private case class Translation(regex: String, ranges: Vector[IntegerRange], hasSeparator: Boolean):

    def raw(text: String): Translation = copy(regex = regex + text)

    def literal(char: Char): Translation =
      copy(regex = regex + quoted(char), hasSeparator = hasSeparator || char == '/')

    def followedBy(next: Translation): Translation =
      Translation(regex + next.regex, ranges ++ next.ranges, hasSeparator || next.hasSeparator)

  private object Translation:
    val Empty: Translation = Translation("", Vector.empty, hasSeparator = false)

  private def quoted(char: Char): String =
    if char.isLetterOrDigit || char > '\u007f' then char.toString else s"\\$char"

  /** `groupsBefore` is how many `{n1..n2}` captures precede `glob` in the whole name, which numbers the next one. */
  private def translate(glob: String, groupsBefore: Int): Translation =
    @tailrec def loop(index: Int, done: Translation): Translation =
      if index >= glob.length then done
      else
        glob.charAt(index) match
          case '\\' if index + 1 < glob.length     => loop(index + 2, done.literal(glob.charAt(index + 1)))
          case '*' if glob.startsWith("**", index) => loop(index + 2, done.raw(".*"))
          case '*'                                 => loop(index + 1, done.raw("[^/]*"))
          case '?'                                 => loop(index + 1, done.raw("[^/]"))
          case '[' =>
            characterClass(glob, index) match
              case Some((regex, next)) => loop(next, done.raw(regex))
              case None                => loop(index + 1, done.literal('['))
          case '{' =>
            braces(glob, index, groupsBefore + done.ranges.size) match
              case Some((alternatives, next)) => loop(next, done.followedBy(alternatives))
              case None                       => loop(index + 1, done.literal('{'))
          case other => loop(index + 1, done.literal(other))
    loop(0, Translation.Empty)

  // A class holding a `/` cannot stand for one path segment character, so its `[` is an ordinary character.
  private def characterClass(glob: String, open: Int): Option[(String, Int)] =
    val negated      = glob.startsWith("!", open + 1)
    val contentStart = if negated then open + 2 else open + 1
    closingBracket(glob, contentStart).flatMap { close =>
      val content = glob.substring(contentStart, close)
      Option.when(content.nonEmpty && !content.contains('/')) {
        val members = classMembers(content)
        (if negated then s"(?!/)[^$members]" else s"[$members]", close + 1)
      }
    }

  @tailrec private def closingBracket(glob: String, index: Int): Option[Int] =
    if index >= glob.length then None
    else
      glob.charAt(index) match
        case ']'  => Some(index)
        case '\\' => closingBracket(glob, index + 2)
        case _    => closingBracket(glob, index + 1)

  private def classMembers(content: String): String =
    @tailrec def loop(index: Int, done: String): String =
      if index >= content.length then done
      else
        content.charAt(index) match
          case '\\' if index + 1 < content.length => loop(index + 2, done + quoted(content.charAt(index + 1)))
          case '-'                                => loop(index + 1, done + "-")
          case other                              => loop(index + 1, done + quoted(other))
    loop(0, "")

  private val NumberRange = """(-?\d+)\.\.(-?\d+)""".r

  private def braces(glob: String, open: Int, groupsBefore: Int): Option[(Translation, Int)] =
    closingBrace(glob, open + 1, depth = 1).map { close =>
      val inner = glob.substring(open + 1, close)
      val translation = inner match
        case NumberRange(first, second) =>
          (first.toLongOption, second.toLongOption) match
            case (Some(a), Some(b)) =>
              Translation(
                """(-?\d+)""",
                Vector(IntegerRange(groupsBefore + 1, a.min(b), a.max(b))),
                hasSeparator = false
              )
            case _ => literalBraces(inner, groupsBefore)
        case _ =>
          splitOnCommas(inner) match
            case alternatives if alternatives.size > 1 => alternation(alternatives, groupsBefore)
            case _                                     => literalBraces(inner, groupsBefore)
      (translation, close + 1)
    }

  private def literalBraces(inner: String, groupsBefore: Int): Translation =
    Translation.Empty.raw("\\{").followedBy(translate(inner, groupsBefore)).raw("\\}")

  private def alternation(alternatives: List[String], groupsBefore: Int): Translation =
    val translated = alternatives.foldLeft(List.empty[Translation]) { (done, alternative) =>
      done :+ translate(alternative, groupsBefore + done.map(_.ranges.size).sum)
    }
    Translation(
      translated.map(_.regex).mkString("(?:", "|", ")"),
      translated.flatMap(_.ranges).toVector,
      translated.exists(_.hasSeparator)
    )

  @tailrec private def closingBrace(glob: String, index: Int, depth: Int): Option[Int] =
    if index >= glob.length then None
    else
      glob.charAt(index) match
        case '\\'              => closingBrace(glob, index + 2, depth)
        case '{'               => closingBrace(glob, index + 1, depth + 1)
        case '}' if depth == 1 => Some(index)
        case '}'               => closingBrace(glob, index + 1, depth - 1)
        case _                 => closingBrace(glob, index + 1, depth)

  private def splitOnCommas(text: String): List[String] =
    @tailrec def loop(index: Int, depth: Int, start: Int, done: List[String]): List[String] =
      if index >= text.length then done :+ text.substring(start)
      else
        text.charAt(index) match
          case '\\'              => loop(index + 2, depth, start, done)
          case '{'               => loop(index + 1, depth + 1, start, done)
          case '}'               => loop(index + 1, depth - 1, start, done)
          case ',' if depth == 0 => loop(index + 1, depth, index + 1, done :+ text.substring(start, index))
          case _                 => loop(index + 1, depth, start, done)
    loop(0, 0, 0, Nil)
