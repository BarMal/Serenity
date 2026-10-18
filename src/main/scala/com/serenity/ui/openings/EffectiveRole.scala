package com.serenity.ui.openings

import com.serenity.richtext.ParagraphRole

/** What layout, paint and dirty tracking treat a paragraph as. Never persisted: the document keeps its
  * [[ParagraphRole]] and a writer never sees this.
  */
enum EffectiveRole:
  /** The role the document stores for the paragraph. */
  case Real(role: ParagraphRole)

  /** A role the paragraph has only because it opens a chapter. */
  case Virtual(role: ParagraphRole)

  /** The role whose geometry this paragraph lays out and paints with. */
  def layoutRole: ParagraphRole = this match
    case Real(role)    => role
    case Virtual(role) => role

  def mapRole(change: ParagraphRole => ParagraphRole): EffectiveRole = this match
    case Real(role)    => Real(change(role))
    case Virtual(role) => Virtual(change(role))
