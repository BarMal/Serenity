package com.serenity.ui.openings

import scala.annotation.tailrec

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{LineSpan, Rope}

/** Which lines an edit can change the chapter-opening status of without touching them.
  *
  * A paragraph opens a chapter because of the title above it, so editing, inserting or removing a title, a scene break
  * or a setext underline, or the blank lines, lower headings and epigraphs between them and the paragraph, can make the
  * next paragraph an opening or stop it being one. Edits to the paragraph that is the opening can pass the status to
  * the paragraph after it. These checks read the text without a heading index or the rules, so they err on the side of
  * naming a line too many: a line named wrongly is measured again, a line missed would keep a stale row count.
  */
object OpeningInvalidation:

  /** How far back or forward from an edit the checks look before giving up and assuming the worst. */
  private val Reach = 64

  private enum Shape:
    case Skippable, Anchor, Other

  /** The lines of `after` outside `span` that must be measured again because of the edit `span` describes: the title
    * above a setext underline that was edited, and the paragraph that is now the next opening candidate.
    */
  def widenedLines(before: Rope, after: Rope, span: LineSpan): Vector[Int] =
    def text(rope: Rope)(line: Int): String = rope.getLine(line).getOrElse("")
    val touched =
      (span.oldFirst to span.oldLast).map(text(before)) ++ (span.newFirst to span.newLast).map(text(after))
    val titleAbove =
      Option.when(touched.exists(isUnderline) && span.newFirst > 0)(span.newFirst - 1).toVector
    val beyondEnd = text(after)(span.newLast + 1)
    val affectsNext =
      touched.exists(isAnchorLike) || isUnderline(beyondEnd) || pendingAbove(text(after), span.newFirst)
    titleAbove ++ (if affectsNext then nextCandidate(text(after), after.lineCount, span.newLast + 1) else Vector.empty)

  /** Whether the rich-text paragraph at `line` is a candidate whose opening status depends on something above it that
    * differs between `before` and `after`, two versions of a document with the same paragraphs count.
    */
  def governorEdited(before: RichTextDocument, after: RichTextDocument, line: Int): Boolean =
    (isCandidate(before.paragraphAt(line)) || isCandidate(after.paragraphAt(line))) &&
      Seq(before, after).exists(document => governorChanged(before, after, document, line))

  /** The paragraphs outside `edit` that `after` and `before`, two versions of a document the edit sets apart, may have
    * given or taken the status of an opening: the next candidate below the edit in each version, when something above
    * it that governs it differs. A line count that changed shifts the paragraphs the versions are compared by, so every
    * such candidate is named.
    */
  def widenedParagraphs(before: RichTextDocument, after: RichTextDocument, edit: LineEdit): Vector[Int] =
    val candidates =
      nextCandidate(after, edit.first + edit.newCount) ++
        nextCandidate(before, edit.first + edit.oldCount).map(_ + edit.delta)
    candidates.distinct.filter(line => edit.delta != 0 || governorEdited(before, after, line))

  /** The first paragraph from `from` on that could be an opening, when no title or scene break comes first. */
  private def nextCandidate(document: RichTextDocument, from: Int): Vector[Int] =
    @tailrec
    def down(line: Int, steps: Int): Vector[Int] =
      document.paragraphAt(line) match
        case None => Vector.empty
        case Some(paragraph) =>
          if steps > Reach then (line until document.paragraphCount).toVector
          else if isAnchor(paragraph) then Vector.empty
          else if isSkippable(paragraph) then down(line + 1, steps + 1)
          else Vector(line)
    down(from, 0)

  private def governorChanged(
    before: RichTextDocument,
    after: RichTextDocument,
    walked: RichTextDocument,
    line: Int
  ): Boolean =
    def differs(index: Int): Boolean = before.paragraphAt(index) != after.paragraphAt(index)
    def anchor(index: Int): Boolean =
      Seq(before, after).exists(document => document.paragraphAt(index).exists(isAnchor))
    @tailrec
    def up(index: Int, steps: Int): Boolean =
      if index < 0 then false
      else if steps > Reach then true
      else
        walked.paragraphAt(index) match
          case Some(paragraph) if isSkippable(paragraph) => differs(index) || up(index - 1, steps + 1)
          case _                                         => anchor(index) && differs(index)
    up(line - 1, 0)

  private def isCandidate(paragraph: Option[RichTextParagraph]): Boolean =
    paragraph.exists { found =>
      val text = found.plainText.trim
      found.role match
        case ParagraphRole.Heading(_) => false
        case _                        => text.nonEmpty && !SceneBreaks.isSceneBreak(text)
    }

  private def isAnchor(paragraph: RichTextParagraph): Boolean =
    paragraph.role match
      case ParagraphRole.Heading(level) => level <= 1 && paragraph.plainText.trim.nonEmpty
      case _                            => SceneBreaks.isSceneBreak(paragraph.plainText.trim)

  private def isSkippable(paragraph: RichTextParagraph): Boolean =
    val heading = paragraph.role match
      case ParagraphRole.Heading(_) => true
      case _                        => false
    !isAnchor(paragraph) && (paragraph.plainText.isBlank || heading || RichTextOpenings.looksLikeEpigraph(paragraph))

  private def shapeOf(line: String): Shape =
    val trimmed = line.trim
    if trimmed.isEmpty || trimmed.startsWith(">") || isLowerHeading(trimmed) then Shape.Skippable
    else if isAnchorLike(trimmed) then Shape.Anchor
    else Shape.Other

  private def isLowerHeading(trimmed: String): Boolean =
    val hashes = trimmed.takeWhile(_ == '#').length
    hashes >= 2 && hashes <= 6 && (trimmed.length == hashes || trimmed.charAt(hashes).isWhitespace)

  private def isAnchorLike(line: String): Boolean =
    val trimmed = line.trim
    trimmed.startsWith("#") || isUnderline(trimmed) || SceneBreaks.isSceneBreak(trimmed)

  private def isUnderline(line: String): Boolean =
    val trimmed = line.trim
    trimmed.nonEmpty && (trimmed.forall(_ == '=') || trimmed.forall(_ == '-'))

  /** Whether the lines above `first`, back to the first that is neither blank nor a lower heading nor a blockquote, end
    * in a title or scene break: the edit is then at or before the opening of something. The line right above is looked
    * past whatever it is, since the edit can change it.
    */
  private def pendingAbove(text: Int => String, first: Int): Boolean =
    @tailrec
    def up(line: Int, steps: Int): Boolean =
      if line < 0 then false
      else if steps > Reach then true
      else
        shapeOf(text(line)) match
          case Shape.Skippable => up(line - 1, steps + 1)
          case Shape.Anchor    => true
          case Shape.Other     => false
    first > 0 && (isAnchorLike(text(first - 1)) || up(first - 2, 0))

  /** The first line from `from` on that could be a paragraph's opening, when no title or scene break comes first. */
  private def nextCandidate(text: Int => String, lineCount: Int, from: Int): Vector[Int] =
    @tailrec
    def down(line: Int, steps: Int): Vector[Int] =
      if line >= lineCount then Vector.empty
      else if steps > Reach then (line until lineCount).toVector
      else
        shapeOf(text(line)) match
          case Shape.Skippable => down(line + 1, steps + 1)
          case Shape.Anchor    => Vector.empty
          case Shape.Other     => Vector(line)
    down(from, 0)
