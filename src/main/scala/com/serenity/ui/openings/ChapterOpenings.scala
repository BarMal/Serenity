package com.serenity.ui.openings

import com.serenity.config.MarkdownViewMode
import com.serenity.document.MarkdownHeadingIndex
import com.serenity.richtext.{ParagraphRole, RichTextDocument}

/** The settings a paragraph's effective role can depend on. */
final case class OpeningContext(dropCapsEnabled: Boolean, virtualDropCaps: Option[VirtualDropCaps] = None)

object ChapterOpenings:

  type Resolver = (RichTextDocument, Int, OpeningContext) => EffectiveRole

  /** The role of `document`'s paragraph at `line` as layout sees it: the paragraph's own role, with a drop cap degraded
    * to body text while drop caps are disabled.
    */
  def roleFor(document: RichTextDocument, line: Int, context: OpeningContext): EffectiveRole =
    val stored = document.paragraphAt(line).map(_.role).getOrElse(ParagraphRole.Body)
    context.virtualDropCaps
      .fold[EffectiveRole](EffectiveRole.Real(stored))(_.roleAt(line, stored))
      .mapRole(degraded(_, context.dropCapsEnabled))

  /** The document keeps the real role, so turning drop caps back on restores the glyph with nothing lost. */
  def degraded(role: ParagraphRole, dropCapsEnabled: Boolean): ParagraphRole =
    role match
      case ParagraphRole.DropCap(_) if !dropCapsEnabled => ParagraphRole.Body
      case other                                        => other

  val standard: Resolver = roleFor

  /** The predicate for [[EpigraphRule.ExplicitOnly]] while no document marks its epigraphs. */
  val noExplicitEpigraphs: Int => Boolean = _ => false

  /** The paragraphs of `document` that open a chapter or follow a scene break, under `rules`.
    *
    * `explicitEpigraph` says whether the paragraph at an index is marked as an epigraph, which only
    * [[EpigraphRule.ExplicitOnly]] reads. It must answer the same for as long as the result is kept up to date with
    * [[updatedOpeningLines]].
    */
  def openingLines(document: RichTextDocument, rules: OpeningRules, explicitEpigraph: Int => Boolean): OpeningLines =
    OpeningScan.scan(document.paragraphCount, rules, RichTextOpenings.classify(document, rules, explicitEpigraph))

  /** `previous`, the openings of the document before `edit`, brought up to date with `document`. Costs the lines the
    * edit touched and the search pending at them, not the document.
    */
  def updatedOpeningLines(
    previous: OpeningLines,
    document: RichTextDocument,
    edit: LineEdit,
    rules: OpeningRules,
    explicitEpigraph: Int => Boolean
  ): OpeningLines =
    OpeningScan.rescan(
      previous,
      edit,
      document.paragraphCount,
      rules,
      RichTextOpenings.classify(document, rules, explicitEpigraph)
    )

  /** Markdown shows openings only where it is rendered: a source view has none. */
  def rendersOpenings(mode: MarkdownViewMode): Boolean =
    mode == MarkdownViewMode.LivePreview || mode == MarkdownViewMode.Read

  def openingLines(
    headings: MarkdownHeadingIndex,
    mode: MarkdownViewMode,
    rules: OpeningRules,
    explicitEpigraph: Int => Boolean
  ): OpeningLines =
    val lineCount = headings.content.lineCount
    if !rendersOpenings(mode) then OpeningLines.none(lineCount, rules)
    else OpeningScan.scan(lineCount, rules, MarkdownOpenings.classify(headings, rules, explicitEpigraph))

  /** As the rich-text [[updatedOpeningLines]], for the Markdown whose lines `headings` indexes. Reparses everything
    * when `headings` itself had to, since an edit that reaches across blocks, a code fence say, can change lines far
    * from it, and redoes every line the reparse of `headings` covered.
    */
  def updatedOpeningLines(
    previous: OpeningLines,
    headings: MarkdownHeadingIndex,
    edit: LineEdit,
    mode: MarkdownViewMode,
    rules: OpeningRules,
    explicitEpigraph: Int => Boolean
  ): OpeningLines =
    val lineCount = headings.content.lineCount
    val classify  = MarkdownOpenings.classify(headings, rules, explicitEpigraph)
    if !rendersOpenings(mode) then OpeningLines.none(lineCount, rules)
    else if headings.parsedWholeText then OpeningScan.scan(lineCount, rules, classify)
    else
      val reparsed = edit.covering(headings.parsedFrom, headings.parsedFrom + headings.parsedLines)
      OpeningScan.rescan(previous, reparsed, lineCount, rules, classify, MarkdownOpenings.startsBlock(headings))

/** A resolver bound to the context it resolves under. Every site that reads a paragraph's role asks one of these. */
final case class ParagraphRoles(context: OpeningContext, resolver: ChapterOpenings.Resolver):

  def roleFor(document: RichTextDocument, line: Int): EffectiveRole = resolver(document, line, context)

  def layoutRole(document: RichTextDocument, line: Int): ParagraphRole = roleFor(document, line).layoutRole

object ParagraphRoles:

  def standard(dropCapsEnabled: Boolean): ParagraphRoles =
    ParagraphRoles(OpeningContext(dropCapsEnabled), ChapterOpenings.standard)

  val enabled: ParagraphRoles = standard(dropCapsEnabled = true)
