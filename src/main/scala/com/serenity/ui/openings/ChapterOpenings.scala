package com.serenity.ui.openings

import com.serenity.richtext.{ParagraphRole, RichTextDocument}

/** The settings a paragraph's effective role can depend on. */
final case class OpeningContext(dropCapsEnabled: Boolean)

object ChapterOpenings:

  type Resolver = (RichTextDocument, Int, OpeningContext) => EffectiveRole

  /** The role of `document`'s paragraph at `line` as layout sees it: the paragraph's own role, with a drop cap degraded
    * to body text while drop caps are disabled.
    */
  def roleFor(document: RichTextDocument, line: Int, context: OpeningContext): EffectiveRole =
    EffectiveRole.Real(
      degraded(document.paragraphAt(line).map(_.role).getOrElse(ParagraphRole.Body), context.dropCapsEnabled)
    )

  /** The document keeps the real role, so turning drop caps back on restores the glyph with nothing lost. */
  def degraded(role: ParagraphRole, dropCapsEnabled: Boolean): ParagraphRole =
    role match
      case ParagraphRole.DropCap(_) if !dropCapsEnabled => ParagraphRole.Body
      case other                                        => other

  val standard: Resolver = roleFor

/** A resolver bound to the context it resolves under. Every site that reads a paragraph's role asks one of these. */
final case class ParagraphRoles(context: OpeningContext, resolver: ChapterOpenings.Resolver):

  def roleFor(document: RichTextDocument, line: Int): EffectiveRole = resolver(document, line, context)

  def layoutRole(document: RichTextDocument, line: Int): ParagraphRole = roleFor(document, line).layoutRole

object ParagraphRoles:

  def standard(dropCapsEnabled: Boolean): ParagraphRoles =
    ParagraphRoles(OpeningContext(dropCapsEnabled), ChapterOpenings.standard)

  val enabled: ParagraphRoles = standard(dropCapsEnabled = true)
