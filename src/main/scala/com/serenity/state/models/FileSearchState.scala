package com.serenity.state.models

final case class FileSearchResult(
    bufferId: BufferId,
    bufferName: String,
    line: Int,
    lineContent: String
)

final case class FileSearchCursor(
    bufferId: BufferId,
    line: Int
)

final case class FileSearchState(
    query: String,
    results: List[FileSearchResult],
    selectedIndex: Int,
    hasMoreResults: Boolean = false,
    nextCursor: Option[FileSearchCursor] = None
):
  def selectedResult: Option[FileSearchResult] =
    if results.isEmpty then None else results.lift(selectedIndex)

  def moveSelection(delta: Int): FileSearchState =
    if results.isEmpty then this
    else
      val raw     = (selectedIndex + delta) % results.length
      val wrapped = if raw < 0 then results.length + raw else raw
      copy(selectedIndex = wrapped)

  def withQuery(q: String): FileSearchState =
    copy(query = q, selectedIndex = 0)
