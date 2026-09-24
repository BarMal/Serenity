package com.serenity.state.reducers

import com.serenity.keystroke.events.*
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
      case LspEvent.LspDiagnosticsReceived(uri, diagnostics) =>
        ReducerResult.noEffects(
          state.copy(runtime =
            state.runtime.copy(diagnosticsState =
              state.runtime.diagnosticsState
                .copy(diagnostics = state.runtime.diagnosticsState.diagnostics + (uri -> diagnostics))
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

      case LspEvent.LspSemanticTokensReceived(uri, tokens) =>
        ReducerResult.noEffects(
          state.copy(runtime =
            state.runtime.copy(semanticTokensState =
              state.runtime.semanticTokensState.copy(
                byUri = state.runtime.semanticTokensState.byUri + (uri -> tokens),
                unavailableUris = state.runtime.semanticTokensState.unavailableUris - uri
              )
            )
          )
        )

      case LspEvent.LspSemanticTokensUnavailable(uri) =>
        ReducerResult.noEffects(
          state.copy(runtime =
            state.runtime.copy(semanticTokensState =
              state.runtime.semanticTokensState.copy(
                byUri = state.runtime.semanticTokensState.byUri - uri,
                unavailableUris = state.runtime.semanticTokensState.unavailableUris + uri
              )
            )
          )
        )
