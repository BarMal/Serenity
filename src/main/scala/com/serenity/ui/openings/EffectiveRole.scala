package com.serenity.ui.openings

import com.serenity.richtext.ParagraphRole

/** What layout, paint and dirty tracking treat a paragraph as. Never persisted: the document keeps its
  * [[ParagraphRole]] and a writer never sees this.
  */
enum EffectiveRole:
  /** The role the document stores for the paragraph. */
  case Real(role: ParagraphRole)

  /** A role the paragraph has only because it opens a chapter, and the shape its first letter is set in. */
  case Virtual(role: ParagraphRole, shape: LetterShape = LetterShape.Drop)

  /** The role whose geometry this paragraph lays out and paints with. */
  def layoutRole: ParagraphRole = this match
    case Real(role)       => role
    case Virtual(role, _) => role

  def letterShape: LetterShape = this match
    case Real(_)         => LetterShape.Drop
    case Virtual(_, how) => how

  def mapRole(change: ParagraphRole => ParagraphRole): EffectiveRole = this match
    case Real(role)         => Real(change(role))
    case Virtual(role, how) => Virtual(change(role), how)
