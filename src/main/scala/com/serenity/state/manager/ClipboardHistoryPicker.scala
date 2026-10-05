package com.serenity.state.manager

import com.serenity.command.ClipboardHistoryCommands
import com.serenity.state.models.*

/** "Paste from History": the recent clipboard texts, newest first, each labelled by its first line. */
object ClipboardHistoryPicker:

  val Title: String = "Paste from History"

  private val LabelLength = 60

  def modalFor(state: AppState): Modal =
    val choices = state.runtime.clipboardHistory.entries.map(entry =>
      ListChoice(label(entry), detail(entry), ClipboardHistoryCommands.paste(entry))
    )
    Modal.ListPicker(ListPicker.of(Title, choices, "Nothing copied yet"))

  private def label(entry: ClipboardEntry): String =
    val firstLine = entry.text.linesIterator.nextOption().getOrElse("")
    if firstLine.length > LabelLength then firstLine.take(LabelLength - 1) + "…" else firstLine

  private def detail(entry: ClipboardEntry): Option[String] =
    val lines = entry.text.count(_ == '\n') + 1
    if entry.wholeLine then Some(if lines == 1 then "line" else s"$lines lines")
    else Option.when(lines > 1)(s"$lines lines")
