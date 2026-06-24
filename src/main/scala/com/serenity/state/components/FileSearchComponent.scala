package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

class FileSearchComponent extends TypedFocusedComponent[ModalInputEvent]:

  private val resultBatchSize: Int = 100

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
    val batch = searchBuffers(newQuery, state, startCursor = None, resultBatchSize)
    val newSearch = FileSearchState(
      query = newQuery,
      results = batch.results,
      selectedIndex = 0,
      hasMoreResults = batch.nextCursor.isDefined,
      nextCursor = batch.nextCursor
    )
    replaceSurface(state, surface, SurfaceContent.FileSearch(newSearch))

  private def updateSelection(
    state: AppState,
    surface: UiSurface,
    searchState: FileSearchState,
    delta: Int
  ): AppState =
    val newSearch =
      if delta > 0 && searchState.selectedIndex == searchState.results.length - 1 && searchState.hasMoreResults then
        appendNextBatch(state, searchState)
      else searchState.moveSelection(delta)
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

  private case class FileSearchBatch(results: List[FileSearchResult], nextCursor: Option[FileSearchCursor])

  private def appendNextBatch(state: AppState, searchState: FileSearchState): FileSearchState =
    searchState.nextCursor match
      case None => searchState.moveSelection(1)
      case Some(cursor) =>
        val batch       = searchBuffers(searchState.query, state, startCursor = Some(cursor), resultBatchSize)
        val nextResults = searchState.results ++ batch.results
        searchState.copy(
          results = nextResults,
          selectedIndex = searchState.results.length.min(math.max(0, nextResults.length - 1)),
          hasMoreResults = batch.nextCursor.isDefined,
          nextCursor = batch.nextCursor
        )

  private def searchBuffers(
    query: String,
    state: AppState,
    startCursor: Option[FileSearchCursor],
    maxResults: Int
  ): FileSearchBatch =
    if query.isEmpty || maxResults <= 0 then FileSearchBatch(Nil, None)
    else
      val lowerQuery = query.toLowerCase
      val buffers    = state.buffers.values.toList.sortBy(_.id.value)
      val matchedBuffers = startCursor match
        case None         => buffers
        case Some(cursor) => buffers.dropWhile(_.id.value < cursor.bufferId.value)

      def startLine(bufferId: BufferId): Int =
        startCursor.filter(_.bufferId == bufferId).map(_.line).getOrElse(0)

      val matches = for
        buffer <- matchedBuffers.iterator
        name = buffer.filePath.map(_.getFileName.toString).getOrElse(s"buffer-${buffer.id.value}")
        (lineIdx, line) <- buffer.content.linesIteratorFrom(startLine(buffer.id))
        if line.toLowerCase.contains(lowerQuery)
      yield FileSearchResult(buffer.id, name, lineIdx, line.trim)

      val loaded = matches.take(maxResults + 1).toList
      loaded.drop(maxResults).headOption match
        case Some(next) => FileSearchBatch(loaded.take(maxResults), Some(FileSearchCursor(next.bufferId, next.line)))
        case None       => FileSearchBatch(loaded, None)
