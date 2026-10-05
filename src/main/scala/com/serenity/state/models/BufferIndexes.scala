package com.serenity.state.models

import java.nio.file.Path

import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{Diagnostic, SemanticToken}
import com.serenity.spellcheck.SpellChecker

/** The per-buffer indexes the renderer reads every frame, declared as derived values (#1864) so they are recomputed
  * when their inputs change rather than by every `AppState` copy. Each reads only its own document's entries, so
  * language data arriving for one file leaves every other buffer's index alone.
  */
object BufferIndexes:

  final case class Source(buffer: Buffer, uri: DocumentUri, languageService: LanguageServiceState)

  final case class UriInputs(bufferId: BufferId, filePath: Option[Path])

  final case class AnnotationInputs(comments: List[DocumentComment], diagnostics: List[Diagnostic])

  final case class SemanticTokenInputs(tokens: Option[List[SemanticToken]], unavailable: Boolean)

  // `Nil` is a real, empty response, so the two answers that carry no token list are stood in for by these.
  private object AwaitingTokens
  private object NoTokensComing

  // `Path.toUri` stats the file on Unix, so a buffer's URI is derived once per path, not once per commit. The buffer
  // id is left out of `references`: a memo is only ever held under its own buffer's id.
  val documentUri: DerivedValue[Buffer, UriInputs, DocumentUri] =
    DerivedValue(
      inputs = buffer => UriInputs(buffer.id, buffer.document.filePath),
      references = inputs => List(inputs.filePath),
      compute = inputs => SpellChecker.diagnosticsUri(inputs.bufferId, inputs.filePath)
    )

  val annotations: DerivedValue[Source, AnnotationInputs, AnnotationLineIndex] =
    DerivedValue(
      inputs = source =>
        AnnotationInputs(
          source.buffer.annotations.documentComments,
          source.languageService.diagnosticsState.diagnostics.getOrElse(source.uri, Nil)
        ),
      references = inputs => List(inputs.comments, inputs.diagnostics),
      compute = inputs =>
        AnnotationLineIndex(inputs.comments.toVector, inputs.diagnostics.groupMap(_.range.start.line)(identity))
    )

  val semanticTokens: DerivedValue[Source, SemanticTokenInputs, SemanticTokensAvailability] =
    DerivedValue(
      inputs = semanticTokenInputs,
      references =
        inputs => List(inputs.tokens.getOrElse(if inputs.unavailable then NoTokensComing else AwaitingTokens)),
      compute = inputs =>
        inputs.tokens match
          case Some(tokens) => SemanticTokensAvailability.Available(tokens.groupBy(_.line))
          case None =>
            if inputs.unavailable then SemanticTokensAvailability.Unavailable
            else SemanticTokensAvailability.Pending
    )

  private def semanticTokenInputs(source: Source): SemanticTokenInputs =
    val tokens = source.languageService.semanticTokensState
    SemanticTokenInputs(tokens.byUri.get(source.uri), tokens.unavailableUris.contains(source.uri))

/** The [[BufferIndexes]] memos an `AppState` carries from copy to copy, one per buffer placed in a pane.
  *
  * Every instance compares equal to every other, as `CommandRunnerSearchCache` does: what it holds is derived from the
  * state's own fields and a stale memo is never served, so a warm cache and a cold one must not make two
  * otherwise-identical states unequal.
  */
final class BufferIndexMemos private (
    val uris: Map[BufferId, Memo[DocumentUri]],
    val annotations: Map[BufferId, Memo[AnnotationLineIndex]],
    val semanticTokens: Map[BufferId, Memo[SemanticTokensAvailability]]
):

  /** `buffer`'s URI, from its memo while that is current. */
  def uriFor(buffer: Buffer): DocumentUri =
    BufferIndexes.documentUri.valueFor(uris.get(buffer.id), buffer)

  /** Memos for exactly `buffers`, reusing every one whose inputs are unchanged; `this` when all of them are. */
  def refreshed(buffers: Map[BufferId, Buffer], languageService: LanguageServiceState): BufferIndexMemos =
    val nextUris = buffers.map((id, buffer) => id -> BufferIndexes.documentUri.refreshed(uris.get(id), buffer))
    val sources = nextUris.flatMap((id, uriMemo) =>
      buffers.get(id).map(buffer => id -> BufferIndexes.Source(buffer, uriMemo.value, languageService))
    )
    val nextAnnotations =
      sources.map((id, source) => id -> BufferIndexes.annotations.refreshed(annotations.get(id), source))
    val nextSemanticTokens =
      sources.map((id, source) => id -> BufferIndexes.semanticTokens.refreshed(semanticTokens.get(id), source))
    if BufferIndexMemos.reused(uris, nextUris) && BufferIndexMemos.reused(annotations, nextAnnotations) &&
        BufferIndexMemos.reused(semanticTokens, nextSemanticTokens)
    then this
    else new BufferIndexMemos(nextUris, nextAnnotations, nextSemanticTokens)

  override def equals(other: Any): Boolean =
    other match
      case _: BufferIndexMemos => true
      case _                   => false

  override def hashCode: Int = 0

  override def toString: String = "BufferIndexMemos"

object BufferIndexMemos:
  val empty: BufferIndexMemos = new BufferIndexMemos(Map.empty, Map.empty, Map.empty)

  private def reused[A](previous: Map[BufferId, Memo[A]], next: Map[BufferId, Memo[A]]): Boolean =
    previous.size == next.size && next.forall((id, memo) => previous.get(id).exists(_ eq memo))
