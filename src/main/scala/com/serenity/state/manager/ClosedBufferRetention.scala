package com.serenity.state.manager

import cats.effect.IO
import com.serenity.lsp.client.DocumentUri
import com.serenity.state.models.{AppState, Buffer}

/** What a commit lets go of for the buffers it removes. Undo history and the language server's per-document data are
  * dropped inside the commit itself, so no state ever holds an entry for a buffer that is gone; the render caches,
  * which live outside the model, are told right after it.
  */
private[manager] object ClosedBufferRetention:

  /** `next` without what the model kept for the buffers `previous` held and `next` no longer does: their undo entries,
    * and the diagnostics and semantic tokens of their documents -- except those another open buffer still shows. The
    * very `next` comes back when no buffer was removed, which is every commit but a close.
    */
  def forgetting(previous: AppState, next: Model): Model =
    val closed = closedBuffers(previous, next.app)
    if closed.isEmpty then next
    else
      val liveBuffers = next.app.persisted.buffers
      val stillShown  = liveBuffers.values.map(next.app.runtime.bufferIndexMemos.uriFor).toSet
      val forgotten   = closed.map(previous.runtime.bufferIndexMemos.uriFor).toSet -- stillShown
      next.copy(
        app = withoutLanguageData(next.app, forgotten),
        undo = next.undo.retainingBuffers(liveBuffers.contains)
      )

  def forgetRenderCaches(caches: RenderCaches)(before: AppState, after: AppState): IO[Unit] =
    if closedBuffers(before, after).isEmpty then IO.unit
    else IO.delay(caches.chapterGhosts.retainOnly(after.persisted.buffers.contains))

  private def closedBuffers(before: AppState, after: AppState): List[Buffer] =
    if before.persisted.buffers eq after.persisted.buffers then Nil
    else
      before.persisted.buffers.iterator.collect {
        case (id, buffer) if !after.persisted.buffers.contains(id) => buffer
      }.toList

  private def withoutLanguageData(state: AppState, documents: Set[DocumentUri]): AppState =
    val languageService = state.runtime.languageService
    val diagnostics     = languageService.diagnosticsState
    val semanticTokens  = languageService.semanticTokensState
    state.copy(runtime =
      state.runtime.copy(languageService =
        languageService.copy(
          diagnosticsState = diagnostics.copy(
            diagnostics = diagnostics.diagnostics -- documents,
            spellCheckCache = diagnostics.spellCheckCache -- documents
          ),
          semanticTokensState = semanticTokens.copy(
            byUri = semanticTokens.byUri -- documents,
            unavailableUris = semanticTokens.unavailableUris -- documents
          )
        )
      )
    )
