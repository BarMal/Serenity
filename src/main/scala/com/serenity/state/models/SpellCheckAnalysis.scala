package com.serenity.state.models

import com.serenity.lsp.model.Diagnostic
import com.serenity.rope.Rope

/** What a line of a document is, as far as spelling is concerned: prose, or markup that is carried over from the line
  * before it.
  */
enum ProseRegion:
  case Prose
  case FrontMatter
  case Fence(marker: Char)

/** `region` is what the lines after `afterLine` are in, up to the next change. */
final case class ProseRegionChange(afterLine: Int, region: ProseRegion)

/** Everything the next check of `content` can start from. A line's words depend only on its text and the region it
  * opens in, so after an edit only the lines the edit touched, and those whose region it changed, are read again.
  */
final case class SpellCheckAnalysis(
    content: Rope,
    regionChanges: Vector[ProseRegionChange],
    unknownWords: List[Diagnostic],
    dictionary: AnyRef
)
