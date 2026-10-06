package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.LspProgressTask
import com.serenity.state.models.AppState
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
        val uri = DocumentUri(rawUri)
        ReducerResult.noEffects(
          state.copy(runtime =
            state.runtime.copy(languageService =
              state.runtime.languageService.copy(diagnosticsState =
                state.runtime.languageService.diagnosticsState
                  .copy(diagnostics = state.runtime.languageService.diagnosticsState.diagnostics + (uri -> diagnostics))
              )
            )
          )
        )

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
        val uri = DocumentUri(rawUri)
        ReducerResult.noEffects(
          state.copy(runtime =
            state.runtime.copy(languageService =
              state.runtime.languageService.copy(semanticTokensState =
                state.runtime.languageService.semanticTokensState.copy(
                  byUri = state.runtime.languageService.semanticTokensState.byUri + (uri -> tokens),
                  unavailableUris = state.runtime.languageService.semanticTokensState.unavailableUris - uri
                )
              )
            )
          )
        )

      case LspEvent.LspSemanticTokensUnavailable(rawUri) =>
        val uri = DocumentUri(rawUri)
        ReducerResult.noEffects(
          state.copy(runtime =
            state.runtime.copy(languageService =
              state.runtime.languageService.copy(semanticTokensState =
                state.runtime.languageService.semanticTokensState.copy(
                  byUri = state.runtime.languageService.semanticTokensState.byUri - uri,
                  unavailableUris = state.runtime.languageService.semanticTokensState.unavailableUris + uri
                )
              )
            )
          )
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
