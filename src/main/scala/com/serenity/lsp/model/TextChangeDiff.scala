package com.serenity.lsp.model

import scala.annotation.tailrec

/** Computes the minimal single-range edit between two full document snapshots, for LSP incremental sync
  * (`TextDocumentContentChangeEvent`). Only the before/after text is available at the call site (the buffer layer hands
  * `LspManager` a full new snapshot, not the edit operation itself), so the change is inferred as the smallest span
  * outside the snapshots' common prefix and suffix -- the same technique most LSP clients use when they only have
  * whole-buffer text to diff. Applying `text` to `oldText` in place of `range` reconstructs `newText` exactly.
  */
object TextChangeDiff:

  final case class Change(range: LspRange, rangeLength: Int, text: String)

  def diff(oldText: String, newText: String): Change =
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

    val suffixLen = dropLeadingLowSurrogate(commonSuffixLength(0))

    val oldEnd = oldText.length - suffixLen
    val newEnd = newText.length - suffixLen

    Change(
      range = LspRange(positionAt(oldText, prefixLen), positionAt(oldText, oldEnd)),
      rangeLength = oldEnd - prefixLen,
      text = newText.substring(prefixLen, newEnd)
    )

  /** LSP positions are 0-based line/character pairs; `character` counts UTF-16 code units into the line, which is
    * exactly what indexing a Scala `String` (UTF-16 `Char`s) by offset already gives.
    */
  private def positionAt(text: String, offset: Int): LspPosition =
    @tailrec def loop(i: Int, line: Int, lineStart: Int): LspPosition =
      if i >= offset then LspPosition(line, offset - lineStart)
      else if text(i) == '\n' then loop(i + 1, line + 1, i + 1)
      else loop(i + 1, line, lineStart)
    loop(0, 0, 0)
