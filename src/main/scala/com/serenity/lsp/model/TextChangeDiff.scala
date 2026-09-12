package com.serenity.lsp.model

/** Computes the minimal single-range edit between two full document snapshots, for LSP incremental sync
  * (`TextDocumentContentChangeEvent`). Only the before/after text is available at the call site (the buffer layer
  * hands `LspManager` a full new snapshot, not the edit operation itself), so the change is inferred as the smallest
  * span outside the snapshots' common prefix and suffix -- the same technique most LSP clients use when they only
  * have whole-buffer text to diff. Applying `text` to `oldText` in place of `range` reconstructs `newText` exactly.
  */
object TextChangeDiff:

  final case class Change(range: LspRange, rangeLength: Int, text: String)

  def diff(oldText: String, newText: String): Change =
    val maxCommon = math.min(oldText.length, newText.length)

    var prefixLen = 0
    while prefixLen < maxCommon && oldText(prefixLen) == newText(prefixLen) do prefixLen += 1

    val maxSuffix = maxCommon - prefixLen
    var suffixLen = 0
    while suffixLen < maxSuffix &&
      oldText(oldText.length - 1 - suffixLen) == newText(newText.length - 1 - suffixLen)
    do suffixLen += 1

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
    var line       = 0
    var lineStart  = 0
    var i          = 0
    while i < offset do
      if text(i) == '\n' then
        line += 1
        lineStart = i + 1
      i += 1
    LspPosition(line, offset - lineStart)
