package com.serenity.state.models

import java.util.Locale

import com.serenity.command.NavigationCommands
import com.serenity.ui.widget.{Loadable, TextField}

/** A line of an open buffer: where a [[BufferTextSearch]] batch resumes. */
final case class BufferLine(bufferId: BufferId, line: Int)

/** "Search in Open Files": the lines of every open buffer that contain a query, found a batch at a time so a common
  * query over large buffers reads only as far as the matches shown.
  */
object BufferTextSearch:

  val BatchSize: Int = 100

  val Title: String = "Search in Open Files"

  val EmptyQueryMessage: String = "Type to search open files"

  val picker: ListPicker =
    ListPicker(
      Title,
      Loadable.Empty(EmptyQueryMessage),
      query = Some(TextField()),
      source = Some(PickerSource.BufferText())
    )

  /** Up to `batchSize` matching lines, ignoring case, from `from` on -- buffers by id, then lines in order -- each
    * picking the command that goes to it; and the first line of the next batch, if any line beyond them matches.
    */
  def batch(
    state: AppState,
    query: String,
    batchSize: Int,
    from: Option[BufferLine]
  ): (Vector[ListChoice], Option[BufferLine]) =
    val needle = query.toLowerCase(Locale.ROOT)
    val size   = batchSize.max(1)
    val buffers = state.persisted.buffers.values.toVector
      .sortBy(_.id.value)
      .dropWhile(buffer => from.exists(start => buffer.id.value < start.bufferId.value))
    val matches = for
      buffer <- buffers.iterator
      firstLine = from.filter(_.bufferId == buffer.id).fold(0)(_.line)
      (line, text) <- buffer.document.content.linesIteratorFrom(firstLine)
      if text.toLowerCase(Locale.ROOT).contains(needle)
    yield BufferLine(buffer.id, line) -> choice(buffer, line, text)
    val loaded = matches.take(size + 1).toVector
    (loaded.take(size).map(_._2), loaded.lift(size).map(_._1))

  private def choice(buffer: Buffer, line: Int, text: String): ListChoice =
    val name = buffer.document.filePath.map(_.getFileName.toString).getOrElse(s"buffer-${buffer.id.value}")
    ListChoice(s"$name:${line + 1}", Some(text.trim), NavigationCommands.goToBufferLine(buffer.id, line))
