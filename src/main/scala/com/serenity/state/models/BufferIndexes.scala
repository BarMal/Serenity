package com.serenity.state.models

import java.nio.file.Path

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.Diagnostic
import com.serenity.spellcheck.SpellChecker

/** The per-buffer indexes the renderer reads every frame, declared as derived values (#1864) so they are recomputed
  * when their inputs change rather than by every `AppState` copy.
  */
object BufferIndexes:

  final case class Source(buffer: Buffer, languageService: LanguageServiceState)

  final case class AnnotationInputs(
      bufferId: BufferId,
      filePath: Option[Path],
      comments: List[DocumentComment],
      diagnostics: Map[DocumentUri, List[Diagnostic]]
  )

  final case class SemanticTokenInputs(bufferId: BufferId, filePath: Option[Path], tokens: SemanticTokensState)

  // The buffer id is left out of `references`: a memo is only ever held under its own buffer's id.
  val annotations: DerivedValue[Source, AnnotationInputs, AnnotationLineIndex] =
    DerivedValue(
      inputs = source =>
        AnnotationInputs(
          source.buffer.id,
          source.buffer.document.filePath,
          source.buffer.annotations.documentComments,
          source.languageService.diagnosticsState.diagnostics
        ),
      references = inputs => List(inputs.filePath, inputs.comments, inputs.diagnostics),
      compute = inputs =>
        AnnotationLineIndex(
          inputs.comments.toVector,
          inputs.diagnostics
            .getOrElse(SpellChecker.diagnosticsUri(inputs.bufferId, inputs.filePath), Nil)
            .groupMap(_.range.start.line)(identity)
        )
    )

  val semanticTokens: DerivedValue[Source, SemanticTokenInputs, SemanticTokensAvailability] =
    DerivedValue(
      inputs = source =>
        SemanticTokenInputs(
          source.buffer.id,
          source.buffer.document.filePath,
          source.languageService.semanticTokensState
        ),
      references = inputs => List(inputs.filePath, inputs.tokens),
      compute = inputs =>
        val uri = SpellChecker.diagnosticsUri(inputs.bufferId, inputs.filePath)
        inputs.tokens.byUri.get(uri) match
          case Some(tokens) => SemanticTokensAvailability.Available(tokens.groupBy(_.line))
          case None =>
            if inputs.tokens.unavailableUris.contains(uri) then SemanticTokensAvailability.Unavailable
            else SemanticTokensAvailability.Pending
    )

/** The [[BufferIndexes]] memos an `AppState` carries from copy to copy, one per buffer placed in a pane.
  *
  * Every instance compares equal to every other, as `CommandRunnerSearchCache` does: what it holds is derived from the
  * state's own fields and a stale memo is never served, so a warm cache and a cold one must not make two
  * otherwise-identical states unequal.
  */
final class BufferIndexMemos private (
    val annotations: Map[BufferId, Memo[AnnotationLineIndex]],
    val semanticTokens: Map[BufferId, Memo[SemanticTokensAvailability]]
):

  /** Memos for exactly `sources`, reusing every one whose inputs are unchanged; `this` when all of them are. */
  def refreshed(sources: Map[BufferId, BufferIndexes.Source]): BufferIndexMemos =
    val nextAnnotations =
      sources.map((id, source) => id -> BufferIndexes.annotations.refreshed(annotations.get(id), source))
    val nextSemanticTokens =
      sources.map((id, source) => id -> BufferIndexes.semanticTokens.refreshed(semanticTokens.get(id), source))
    if BufferIndexMemos.reused(annotations, nextAnnotations) &&
        BufferIndexMemos.reused(semanticTokens, nextSemanticTokens)
    then this
    else new BufferIndexMemos(nextAnnotations, nextSemanticTokens)

  override def equals(other: Any): Boolean =
    other match
      case _: BufferIndexMemos => true
      case _                   => false

  override def hashCode: Int = 0

  override def toString: String = "BufferIndexMemos"

object BufferIndexMemos:
  val empty: BufferIndexMemos = new BufferIndexMemos(Map.empty, Map.empty)

  private def reused[A](previous: Map[BufferId, Memo[A]], next: Map[BufferId, Memo[A]]): Boolean =
    previous.size == next.size && next.forall((id, memo) => previous.get(id).exists(_ eq memo))
