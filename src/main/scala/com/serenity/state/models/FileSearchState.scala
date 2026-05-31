package com.serenity.state.models

case class FileSearchResult(
    bufferId: BufferId,
    bufferName: String,
    line: Int,
    lineContent: String
)

case class FileSearchState(
    query: String,
    results: List[FileSearchResult],
    selectedIndex: Int
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
