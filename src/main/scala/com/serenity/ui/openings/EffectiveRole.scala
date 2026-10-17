package com.serenity.ui.openings

import com.serenity.richtext.ParagraphRole

/** What layout, paint and dirty tracking treat a paragraph as. Never persisted: the document keeps its
  * [[ParagraphRole]] and a writer never sees this.
  */
enum EffectiveRole:
  case Real(role: ParagraphRole)

  /** The role whose geometry this paragraph lays out and paints with. */
  def layoutRole: ParagraphRole = this match
    case Real(role) => role
