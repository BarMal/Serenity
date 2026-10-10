package com.serenity.lsp.model

import scala.annotation.tailrec

import com.serenity.rope.{Balance, ChangeSet, Replacement, Rope, RopeDiff}

/** Computes the minimal single-range edit between two full document snapshots, for LSP incremental sync
  * (`TextDocumentContentChangeEvent`). Only the before/after text is available at the call site (the buffer layer hands
  * `LspManager` a full new snapshot, not the edit operation itself), so the change is inferred as the smallest span
  * outside the snapshots' common prefix and suffix -- the same technique most LSP clients use when they only have
  * whole-buffer text to diff. Applying `text` to `oldText` in place of `range` reconstructs `newText` exactly.
  */
object TextChangeDiff:

  final case class Change(range: LspRange, rangeLength: Int, text: String)

  def diff(oldText: String, newText: String): Change =
    val (prefixLen, suffixLen) = commonAffixes(oldText, newText)
    val oldEnd                 = oldText.length - suffixLen
    val newEnd                 = newText.length - suffixLen

    Change(
      range = LspRange(positionAt(oldText, prefixLen), positionAt(oldText, oldEnd)),
      rangeLength = oldEnd - prefixLen,
      text = newText.substring(prefixLen, newEnd)
    )

  /** The same change as [[diff]] over the texts of `before` and `after`, without building either text. [[RopeDiff]]
    * finds a window of `after` that contains every difference by walking the subtrees the edit left shared, so only the
    * window is read; the exact prefix and suffix are then settled inside it, and positions come from the rope's own
    * line index. A window can be wider than the edit but never narrower (see [[RopeDiff]]), so the result is what
    * [[diff]] reports for the whole texts.
    */
  def diff(before: Rope, after: Rope)(using Balance): Change =
    val (changedStart, changedEnd) = RopeDiff.changedOffsetRange(before, after).getOrElse((after.weight, after.weight))
    val start                      = keepingPairsTogetherBefore(after, changedStart)
    val end                        = keepingPairsTogetherAfter(after, changedEnd)
    val beforeEnd                  = end + before.weight - after.weight
    val (prefixLen, suffixLen) =
      commonAffixes(before.sliceString(start, beforeEnd), after.sliceString(start, end))
    val oldStart = start + prefixLen
    val oldEnd   = beforeEnd - suffixLen

    Change(
      range = LspRange(positionIn(before, oldStart), positionIn(before, oldEnd)),
      rangeLength = oldEnd - oldStart,
      text = after.sliceString(oldStart, end - suffixLen)
    )

  /** `change`, an edit of `before` already known, as the content changes a server applies one after another. Taken last
    * part first, so each range is still in the coordinates of the text it applies to; positions come from the rope's
    * own line index, and nothing is compared.
    */
  def changes(before: Rope, change: ChangeSet): List[Change] =
    change.parts.reverseIterator.map(changeOf(before, _)).toList

  private def changeOf(before: Rope, part: Replacement): Change =
    Change(
      range = LspRange(positionIn(before, part.from), positionIn(before, part.to)),
      rangeLength = part.removedLength,
      text = part.insert
    )

  /** The window starts after the last high surrogate it would otherwise split from its low half. */
  @tailrec
  private def keepingPairsTogetherBefore(text: Rope, offset: Int): Int =
    if offset > 0 && text.index(offset - 1).exists(char => Character.isHighSurrogate(char)) then
      keepingPairsTogetherBefore(text, offset - 1)
    else offset

  @tailrec
  private def keepingPairsTogetherAfter(text: Rope, offset: Int): Int =
    if text.index(offset).exists(char => Character.isLowSurrogate(char)) then
      keepingPairsTogetherAfter(text, offset + 1)
    else offset

  /** The lengths of the common prefix and suffix of the two texts, the suffix taken from what the prefix leaves. */
  private def commonAffixes(oldText: String, newText: String): (Int, Int) =
    val maxCommon = math.min(oldText.length, newText.length)

    @tailrec def commonPrefixLength(len: Int): Int =
      if len < maxCommon && oldText(len) == newText(len) then commonPrefixLength(len + 1) else len

    // A high surrogate at the last common position means its low surrogate half was excluded from the common
    // prefix (it either didn't match or fell outside maxCommon) -- back off one unit so the pair stays together
    // in the changed middle instead of splitting.
    @tailrec def dropTrailingHighSurrogate(len: Int): Int =
      if len > 0 && Character.isHighSurrogate(oldText(len - 1)) then dropTrailingHighSurrogate(len - 1) else len

    val prefixLen = dropTrailingHighSurrogate(commonPrefixLength(0))
    val maxSuffix = maxCommon - prefixLen

    @tailrec def commonSuffixLength(len: Int): Int =
      if len < maxSuffix && oldText(oldText.length - 1 - len) == newText(newText.length - 1 - len)
      then commonSuffixLength(len + 1)
      else len

    // Symmetric case: a low surrogate at the start of the common suffix means its high surrogate half was
    // excluded -- back off one unit so the pair stays together in the changed middle.
    @tailrec def dropLeadingLowSurrogate(len: Int): Int =
      if len > 0 && Character.isLowSurrogate(oldText(oldText.length - len)) then dropLeadingLowSurrogate(len - 1)
      else len

    (prefixLen, dropLeadingLowSurrogate(commonSuffixLength(0)))

  private def positionIn(text: Rope, offset: Int): LspPosition =
    val (line, character) = text.offsetToLineColumn(offset)
    LspPosition(line, character)

  /** LSP positions are 0-based line/character pairs; `character` counts UTF-16 code units into the line, which is
    * exactly what indexing a Scala `String` (UTF-16 `Char`s) by offset already gives.
    */
  private def positionAt(text: String, offset: Int): LspPosition =
    @tailrec def loop(i: Int, line: Int, lineStart: Int): LspPosition =
      if i >= offset then LspPosition(line, offset - lineStart)
      else if text(i) == '\n' then loop(i + 1, line + 1, i + 1)
      else loop(i + 1, line, lineStart)
    loop(0, 0, 0)
