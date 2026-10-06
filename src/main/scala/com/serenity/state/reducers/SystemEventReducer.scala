package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.LspProgressTask
import com.serenity.state.models.{AppState, LanguageServiceState}
import com.serenity.ui.layout.*

object SystemEventReducer:

  def reduce(event: SystemEvent, state: AppState): ReducerResult =
    event match
      case ResizeEvent(newSize) =>
        val synced = LayoutEngine.syncViewportDimensions(state, newSize)
        ReducerResult.noEffects(
          synced.copy(runtime = synced.runtime.copy(viewportSize = Some(newSize)))
        )

      case lsp: LspEvent => reduceLspEvent(lsp, state)

      case _ =>
        ReducerResult.noEffects(state)

  private def reduceLspEvent(event: LspEvent, state: AppState): ReducerResult =
    event match
      case LspEvent.LspDiagnosticsReceived(rawUri, diagnostics) =>
        withOpenDocumentData(rawUri, state) { (uri, languageService) =>
          val held = languageService.diagnosticsState
          languageService.copy(diagnosticsState = held.copy(diagnostics = held.diagnostics + (uri -> diagnostics)))
        }

      case LspEvent.LspHoverReceived(text, anchor) =>
        PeekStateReducer.show(PeekContent.QuickInfo(text), anchor, state)

      case LspEvent.LspCompletionReceived(items, anchor) =>
        val text = items match
          case Nil        => "No completions available."
          case candidates => candidates.mkString("\n")
        PeekStateReducer.show(PeekContent.QuickInfo(text), anchor, state)

      case LspEvent.LspDefinitionReceived(symbol, uri, position, anchor) =>
        PeekStateReducer.show(
          PeekContent.SymbolDefinition(s"$symbol @ $uri", Location(position.line, position.character)),
          anchor,
          state
        )

      case LspEvent.LspReferencesReceived(symbol, locations, anchor) =>
        val text =
          if locations.isEmpty then s"No references found for $symbol."
          else
            locations
              .map { case (uri, position) => s"$uri:${position.line + 1}:${position.character + 1}" }
              .mkString("\n")
        PeekStateReducer.show(PeekContent.QuickInfo(text), anchor, state)

      case LspEvent.LspRenameReceived(edits, anchor) =>
        RenameEditReducer.apply(edits, anchor, state)

      case LspEvent.LspProgressReceived(languageId, token, progress) =>
        withProgress(state, languageId, LspProgressTask.advance(_, token, progress))

      case LspEvent.LspServerStopped(languageId) => withProgress(state, languageId, _ => Nil)

      case LspEvent.LspWorkspaceEditRequested(edits) => WorkspaceEditReducer(edits, state)

      case LspEvent.LspSemanticTokensReceived(rawUri, tokens) =>
        withOpenDocumentData(rawUri, state) { (uri, languageService) =>
          val held = languageService.semanticTokensState
          languageService.copy(semanticTokensState =
            held.copy(byUri = held.byUri + (uri -> tokens), unavailableUris = held.unavailableUris - uri)
          )
        }

      case LspEvent.LspSemanticTokensUnavailable(rawUri) =>
        withOpenDocumentData(rawUri, state) { (uri, languageService) =>
          val held = languageService.semanticTokensState
          languageService.copy(semanticTokensState =
            held.copy(byUri = held.byUri - uri, unavailableUris = held.unavailableUris + uri)
          )
        }

  // A server may still publish for a document after it was closed (an in-flight publish, a re-publish on didClose);
  // nothing reads language data for a document without a buffer, so it would only leak.
  private def withOpenDocumentData(rawUri: String, state: AppState)(
    update: (DocumentUri, LanguageServiceState) => LanguageServiceState
  ): ReducerResult =
    val uri    = DocumentUri(rawUri)
    val isOpen = state.persisted.buffers.values.exists(state.runtime.bufferIndexMemos.uriFor(_) == uri)
    ReducerResult.noEffects(
      if isOpen then
        state.copy(runtime = state.runtime.copy(languageService = update(uri, state.runtime.languageService)))
      else state
    )

  private def withProgress(
    state: AppState,
    languageId: LanguageId,
    update: List[LspProgressTask] => List[LspProgressTask]
  ): ReducerResult =
    val languageService = state.runtime.languageService
    val tasks           = update(languageService.progress.getOrElse(languageId, Nil))
    val progress =
      if tasks.isEmpty then languageService.progress - languageId
      else languageService.progress.updated(languageId, tasks)
    ReducerResult.noEffects(
      state.copy(runtime = state.runtime.copy(languageService = languageService.copy(progress = progress)))
    )
