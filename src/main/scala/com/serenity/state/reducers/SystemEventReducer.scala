package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, SurfaceContent, SurfacePresentation}
import com.serenity.ui.layout.*

object SystemEventReducer:

  def reduce(event: SystemEvent, state: AppState): ReducerResult =
    event match
      case ResizeEvent(newSize) =>
        val newLayout = LayoutEngine.calculateLayout(state, newSize)
        val updatedBuffers = state.layout.editorPanes.values.foldLeft(state.buffers) { (buffers, pane) =>
          pane.bufferId.flatMap(buffers.get) match
            case Some(buffer) =>
              val updatedViewport =
                LayoutEngine.updateViewportDimensions(buffer.viewport, newLayout.editorPanelRect)
              buffers + (buffer.id -> buffer.copy(viewport = updatedViewport))
            case None =>
              buffers
        }

        ReducerResult.noEffects(
          state.copy(
            buffers = updatedBuffers,
            viewportSize = Some(newSize)
          )
        )

      case LspEvent.LspDiagnosticsReceived(uri, diagnostics) =>
        ReducerResult.noEffects(state.copy(diagnostics = state.diagnostics + (uri -> diagnostics)))

      case LspEvent.LspHoverReceived(text, anchor) =>
        PeekStateReducer.show(PeekContent.QuickInfo(text), anchor, state)

      case LspEvent.LspDefinitionReceived(symbol, uri, position, anchor) =>
        PeekStateReducer.show(
          PeekContent.SymbolDefinition(s"$symbol @ $uri", Location(position.line, position.character)),
          anchor,
          state
        )

      case ExplorerEvent.RootDirectoryLoaded(position, rootPath, size, entries, selectedPath) =>
        val tree = DirectoryTreeData(rootPath, entries = Map(rootPath -> entries))
        PanelStateReducer.pin(
          PanelContent.DirectoryTree(tree, selectedPath),
          position,
          size,
          state
        )

      case ExplorerEvent.DirectoryLoaded(position, path, entries) =>
        ReducerResult.noEffects(updatePinnedDirectoryTree(state, position, path, entries))

      case _ =>
        ReducerResult.noEffects(state)

  private def updatePinnedDirectoryTree(
    state: AppState,
    position: PanelPosition,
    path: java.nio.file.Path,
    entries: List[com.serenity.ui.layout.DirEntry]
  ): AppState =
    state.pinnedSurfaces
      .find {
        _.presentation match
          case SurfacePresentation.Pinned(pos, _) if pos == position => true
          case _                                                     => false
      }
      .map { surface =>
        val updatedSurface = surface.content match
          case SurfaceContent.DirectoryTree(tree, selectedPath) =>
            val nextSelected =
              if selectedPath.contains(path) || selectedPath.isEmpty then Some(path)
              else selectedPath
            surface.copy(
              content = SurfaceContent.DirectoryTree(
                tree.copy(
                  expandedPaths = tree.expandedPaths + path,
                  entries = tree.entries + (path -> entries)
                ),
                nextSelected
              )
            )
          case _ =>
            val tree = DirectoryTreeData(
              rootPath = path,
              entries = Map(path -> entries)
            )
            surface.copy(content = SurfaceContent.DirectoryTree(tree, Some(path)))
        state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface)
      }
      .getOrElse(state)
