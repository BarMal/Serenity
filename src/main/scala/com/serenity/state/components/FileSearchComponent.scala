package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

class FileSearchComponent extends TypedFocusedComponent[ModalInputEvent]:

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, state: AppState): ComponentResult =
    state.fileSearchSurface match
      case None => ComponentResult.dismiss
      case Some(surface) =>
        surface.content match
          case SurfaceContent.FileSearch(searchState) =>
            event match
              case ModalInsertChar(c) =>
                ComponentResult.updateState(_ => updateQuery(state, surface, searchState.query + c))
              case ModalDeleteBackward =>
                val newQuery = if searchState.query.isEmpty then "" else searchState.query.dropRight(1)
                ComponentResult.updateState(_ => updateQuery(state, surface, newQuery))
              case ModalDeleteForward =>
                ComponentResult.updateState(_ => updateQuery(state, surface, searchState.query))
              case ModalDeleteWordBackward =>
                ComponentResult.updateState(_ =>
                  updateQuery(state, surface, TextEditing.deleteWordBackward(searchState.query))
                )
              case ModalDeleteWordForward =>
                ComponentResult.updateState(_ =>
                  updateQuery(state, surface, TextEditing.deleteWordForward(searchState.query))
                )
              case ModalNavigate(Direction.Up) =>
                ComponentResult.updateState(_ => updateSelection(state, surface, searchState, -1))
              case ModalNavigate(Direction.Down) =>
                ComponentResult.updateState(_ => updateSelection(state, surface, searchState, 1))
              case ModalSubmit =>
                ComponentResult.updateState(_ => submitOrDismiss(state, surface, searchState))
              case ModalDismiss =>
                ComponentResult.updateState(_ => dismiss(state, surface))
              case _ => ComponentResult.noChange
          case _ => ComponentResult.noChange

  private def updateQuery(state: AppState, surface: UiSurface, newQuery: String): AppState =
    val results   = searchBuffers(newQuery, state)
    val newSearch = FileSearchState(newQuery, results, 0)
    replaceSurface(state, surface, SurfaceContent.FileSearch(newSearch))

  private def updateSelection(
    state: AppState,
    surface: UiSurface,
    searchState: FileSearchState,
    delta: Int
  ): AppState =
    val newSearch = searchState.moveSelection(delta)
    replaceSurface(state, surface, SurfaceContent.FileSearch(newSearch))

  private def submitOrDismiss(state: AppState, surface: UiSurface, searchState: FileSearchState): AppState =
    searchState.selectedResult match
      case Some(result) => navigateToResult(state, surface, result)
      case None         => dismiss(state, surface)

  private def navigateToResult(state: AppState, surface: UiSurface, result: FileSearchResult): AppState =
    val updatedBuffers = state.buffers.get(result.bufferId).fold(state.buffers) { buffer =>
      state.buffers + (result.bufferId -> buffer.copy(cursors = List(CursorPosition(result.line, 0))))
    }
    val withoutSearch = state.copy(
      uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id),
      buffers = updatedBuffers
    )
    withoutSearch.layout.editorPanes.find(_._2.bufferId.contains(result.bufferId)) match
      case Some((paneId, _)) =>
        withoutSearch.copy(
          focus = Focus.EditorPane(paneId),
          layout = withoutSearch.layout.copy(activeEditorPaneId = Some(paneId))
        )
      case None =>
        withoutSearch.layout.activeEditorPaneId match
          case Some(paneId) => withoutSearch.copy(focus = Focus.EditorPane(paneId))
          case None         => withoutSearch

  private def dismiss(state: AppState, surface: UiSurface): AppState =
    val withoutSearch = state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id))
    withoutSearch.layout.activeEditorPaneId match
      case Some(paneId) => withoutSearch.copy(focus = Focus.EditorPane(paneId))
      case None         => withoutSearch

  private def replaceSurface(state: AppState, surface: UiSurface, newContent: SurfaceContent): AppState =
    val newSurface = surface.copy(content = newContent)
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ newSurface)

  private def searchBuffers(query: String, state: AppState): List[FileSearchResult] =
    if query.isEmpty then Nil
    else
      val lowerQuery = query.toLowerCase
      (for
        buffer <- state.buffers.values.toList.sortBy(_.id.value).iterator
        name = buffer.filePath.map(_.getFileName.toString).getOrElse(s"buffer-${buffer.id.value}")
        lineIdx <- (0 until buffer.content.lineCount).iterator
        line = buffer.content.getLine(lineIdx).getOrElse("")
        if line.toLowerCase.contains(lowerQuery)
      yield FileSearchResult(buffer.id, name, lineIdx, line.trim)).take(100).toList
