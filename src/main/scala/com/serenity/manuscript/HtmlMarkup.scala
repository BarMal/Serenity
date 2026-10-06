package com.serenity.manuscript

import java.util.Locale
import java.util.regex.{Matcher, Pattern}

import scala.annotation.tailrec
import scala.util.Try

import com.serenity.richtext.{InlineMark, RichTextRun, RichTextStyle}

/** What a manuscript does with raw HTML in Markdown. The model has no HTML, so tags are read for their meaning and
  * never kept: `b`/`strong`, `i`/`em` and `u` toggle a mark, `br` is a line break, `hr` a scene break, and any other
  * tag is stripped with its text kept. The model has no superscript or subscript, so `sup` and `sub` keep their text as
  * plain text. Comments, scripts and style sheets are not prose and are dropped.
  */
object HtmlMarkup:

  /** What an inline tag does to the runs around it. */
  enum Effect:
    case Open(mark: InlineMark)
    case Close(mark: InlineMark)
    case LineBreak
    case Strip

  /** One paragraph of a block of HTML, or a scene break. */
  enum Part:
    case Text(runs: List[RichTextRun])
    case SceneBreak

  private val TagPattern     = Pattern.compile("""</?[A-Za-z][^<>]*>""")
  private val NamePattern    = Pattern.compile("""^<(/?)([A-Za-z][A-Za-z0-9]*)""")
  private val DroppedPattern = Pattern.compile("""(?is)<!--.*?-->|<(script|style)\b.*?</\1\s*>""")

  private val BlockTags = Set(
    "address",
    "article",
    "aside",
    "blockquote",
    "center",
    "dd",
    "details",
    "div",
    "dl",
    "dt",
    "figcaption",
    "figure",
    "footer",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "header",
    "li",
    "main",
    "nav",
    "ol",
    "p",
    "pre",
    "section",
    "summary",
    "table",
    "td",
    "th",
    "tr",
    "ul"
  )

  def effect(tag: String): Effect =
    val matcher = NamePattern.matcher(tag)
    if !matcher.find() then Effect.Strip
    else
      val closing = matcher.group(1).nonEmpty
      matcher.group(2).toLowerCase(Locale.ROOT) match
        case "b" | "strong" => mark(InlineMark.Bold, closing)
        case "i" | "em"     => mark(InlineMark.Italic, closing)
        case "u"            => mark(InlineMark.Underline, closing)
        case "br"           => Effect.LineBreak
        case _              => Effect.Strip

  private def mark(inline: InlineMark, closing: Boolean): Effect =
    if closing then Effect.Close(inline) else Effect.Open(inline)

  def applied(style: RichTextStyle, effect: Effect): RichTextStyle =
    effect match
      case Effect.Open(mark)  => style.withMark(mark)
      case Effect.Close(mark) => style.withoutMark(mark)
      case _                  => style

  /** The paragraphs a block of HTML holds: its text, whitespace collapsed to single spaces, split where a block-level
    * tag starts or ends.
    */
  def parts(html: String): List[Part] =
    val source  = DroppedPattern.matcher(html).replaceAll("")
    val matcher = TagPattern.matcher(source)
    @tailrec
    def loop(from: Int, state: State): State =
      if matcher.find() then
        val withText = state.text(decoded(source.substring(from, matcher.start)))
        loop(matcher.end, withText.tag(matcher.group))
      else state.text(decoded(source.substring(from)))
    loop(0, State.empty).closed.parts.toList

  final private case class State(style: RichTextStyle, current: Vector[RichTextRun], parts: Vector[Part]):

    def text(raw: String): State =
      val collapsed = raw.replaceAll("\\s+", " ")
      if collapsed.isEmpty then this else copy(current = current :+ RichTextRun(collapsed, style))

    def tag(literal: String): State =
      val name = tagName(literal)
      if name == "hr" then closed.copy(parts = closed.parts :+ Part.SceneBreak)
      else if BlockTags.contains(name) then closed
      else
        effect(literal) match
          case Effect.LineBreak => copy(current = current :+ RichTextRun("\n", style))
          case other            => copy(style = applied(style, other))

    def closed: State =
      val runs = State.trimmed(current)
      copy(current = Vector.empty, parts = if runs.isEmpty then parts else parts :+ Part.Text(runs.toList))

  private object State:
    val empty: State = State(RichTextStyle.empty, Vector.empty, Vector.empty)

    def trimmed(runs: Vector[RichTextRun]): Vector[RichTextRun] =
      val leading = runs.headOption.fold(runs)(first => runs.updated(0, first.copy(text = first.text.stripLeading)))
      val both = leading.lastOption.fold(leading)(last =>
        leading.updated(leading.length - 1, last.copy(text = last.text.stripTrailing))
      )
      both.filter(_.text.nonEmpty)

  private def tagName(literal: String): String =
    val matcher = NamePattern.matcher(literal)
    if matcher.find() then matcher.group(2).toLowerCase(Locale.ROOT) else ""

  private val Entities = Map("amp" -> "&", "lt" -> "<", "gt" -> ">", "quot" -> "\"", "apos" -> "'", "nbsp" -> " ")

  private val EntityPattern = Pattern.compile("&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z]+);")

  private def decoded(text: String): String =
    EntityPattern.matcher(text).replaceAll { found =>
      val body = found.group(1)
      val replacement =
        if body.startsWith("#x") || body.startsWith("#X") then codePoint(body.drop(2), 16)
        else if body.startsWith("#") then codePoint(body.drop(1), 10)
        else Entities.get(body)
      Matcher.quoteReplacement(replacement.getOrElse(found.group))
    }

  private def codePoint(digits: String, radix: Int): Option[String] =
    Try(Integer.parseInt(digits, radix)).toOption
      .filter(point => point > 0 && Character.isValidCodePoint(point))
      .map(point => String(Character.toChars(point)))
