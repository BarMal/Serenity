package com.serenity.lsp.model

/** One edit of a `textDocument/semanticTokens/full/delta` response: replace `deleteCount` integers of the previous
  * result's `data`, starting at integer offset `start`, with `data` (LSP 3.17 §3.17.7.5).
  */
final case class SemanticTokensEdit(start: Int, deleteCount: Int, data: IArray[Int])

/** The wire answer to a semantic tokens request, still in the protocol's relative integer encoding: the `data` of a
  * full or range result, or the edits of a delta result, each with the `resultId` a later delta request refers to.
  */
enum SemanticTokensResult:
  def resultId: Option[String]

  case Full(resultId: Option[String], data: IArray[Int])
  case Delta(resultId: Option[String], edits: List[SemanticTokensEdit])

object SemanticTokensResult:

  /** `previous` with `edits` applied. The edits all refer to offsets of `previous`, so they are applied in order of
    * `start`; `None` when they overlap or reach past its end, which means the server and this client no longer agree on
    * what `previous` is.
    */
  def applyEdits(previous: IArray[Int], edits: List[SemanticTokensEdit]): Option[IArray[Int]] =
    val folded = edits.sortBy(_.start).foldLeft(Option((0, IArray.empty[Int]))) {
      case (Some((consumed, built)), edit)
          if edit.start >= consumed && edit.deleteCount >= 0 && edit.start + edit.deleteCount <= previous.length =>
        Some((edit.start + edit.deleteCount, built ++ previous.slice(consumed, edit.start) ++ edit.data))
      case _ => None
    }
    folded.map((consumed, built) => built ++ previous.slice(consumed, previous.length))
