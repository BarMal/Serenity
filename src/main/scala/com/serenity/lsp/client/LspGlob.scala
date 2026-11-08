package com.serenity.lsp.client

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.util.Try

/** A glob in the syntax of LSP 3.17 file-watcher registrations, matched against a `/`-separated path: `*` and `?` stay
  * within a segment, `**` spans any number of segments including none, `{a,b}` groups alternatives, `[...]` is a
  * character range and `[!...]` its negation. An unclosed group or range is literal text.
  *
  * Not `io.EditorConfigGlob`: there a pattern without a `/` matches at any depth, here it matches only at the top.
  */
final case class LspGlob private (pattern: String):

  // Defensive: translation only emits valid expressions, but a server's pattern must never be able to throw here.
  private lazy val compiled: Option[Pattern] = Try(Pattern.compile(LspGlob.translate(pattern))).toOption

  def matches(path: String): Boolean = compiled.exists(_.matcher(path).matches())

object LspGlob:

  def compile(pattern: String): LspGlob = LspGlob(pattern)

  private def quoted(char: Char): String =
    if char.isLetterOrDigit || char > '\u007f' then char.toString else s"\\$char"

  private def translate(glob: String): String =
    @tailrec def loop(index: Int, done: String): String =
      if index >= glob.length then done
      else
        glob.charAt(index) match
          case '*' if glob.startsWith("**/", index) => loop(index + 3, done + "(?:.*/)?")
          case '*' if glob.startsWith("**", index)  => loop(index + 2, done + ".*")
          case '*'                                  => loop(index + 1, done + "[^/]*")
          case '?'                                  => loop(index + 1, done + "[^/]")
          case '[' =>
            characterClass(glob, index) match
              case Some((regex, next)) => loop(next, done + regex)
              case None                => loop(index + 1, done + quoted('['))
          case '{' =>
            closingBrace(glob, index + 1, depth = 1) match
              case Some(close) =>
                val alternatives = splitOnCommas(glob.substring(index + 1, close)).map(translate)
                loop(close + 1, done + alternatives.mkString("(?:", "|", ")"))
              case None => loop(index + 1, done + quoted('{'))
          case other => loop(index + 1, done + quoted(other))
    loop(0, "")

  // A range holding a `/` cannot stand for one character of a segment, so its `[` is literal.
  private def characterClass(glob: String, open: Int): Option[(String, Int)] =
    val negated      = glob.startsWith("!", open + 1)
    val contentStart = if negated then open + 2 else open + 1
    Option(glob.indexOf(']', contentStart)).filter(_ >= 0).flatMap { close =>
      val content = glob.substring(contentStart, close)
      Option.when(content.nonEmpty && !content.contains('/')) {
        val members = content.map(char => if char == '-' then "-" else quoted(char)).mkString
        (if negated then s"[^/$members]" else s"[$members]", close + 1)
      }
    }

  @tailrec private def closingBrace(glob: String, index: Int, depth: Int): Option[Int] =
    if index >= glob.length then None
    else
      glob.charAt(index) match
        case '{'               => closingBrace(glob, index + 1, depth + 1)
        case '}' if depth == 1 => Some(index)
        case '}'               => closingBrace(glob, index + 1, depth - 1)
        case _                 => closingBrace(glob, index + 1, depth)

  /** The alternatives of a group, split only on commas outside nested groups. */
  private def splitOnCommas(inner: String): List[String] =
    @tailrec def loop(index: Int, depth: Int, start: Int, done: List[String]): List[String] =
      if index >= inner.length then done :+ inner.substring(start)
      else
        inner.charAt(index) match
          case '{'               => loop(index + 1, depth + 1, start, done)
          case '}'               => loop(index + 1, depth - 1, start, done)
          case ',' if depth == 0 => loop(index + 1, depth, index + 1, done :+ inner.substring(start, index))
          case _                 => loop(index + 1, depth, start, done)
    loop(0, 0, 0, Nil)
