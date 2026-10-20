package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicReference

import com.serenity.document.ChapterGhosts
import com.serenity.rope.Rope
import com.serenity.state.models.{Buffer, BufferId, NoteKey, Notes}

/** Remembers each buffer's [[ChapterGhosts]] until its text, its notes, or a note's own text changes, so the heading
  * parse behind them runs once per edit rather than once per painted frame. An entry is reused only when it was built
  * from the very same `Rope` objects: edit paths do not all bump `Document.contentVersion`, so object identity is the
  * one signal that cannot go stale.
  */
final class ChapterGhostCache:

  final private case class Entry(
      content: Rope,
      notes: Map[NoteKey, Notes],
      noteContents: Map[BufferId, Rope],
      overviews: Map[BufferId, (Rope, List[String])],
      ghosts: Map[Int, String]
  )

  private val entries = new AtomicReference(Map.empty[BufferId, Entry])

  def retainOnly(live: BufferId => Boolean): Unit =
    val _ = entries.updateAndGet(_.filter((id, _) => live(id)))

  private[manager] def entryCount: Int = entries.get().size

  def ghostsFor(buffer: Buffer, buffers: Map[BufferId, Buffer]): Map[Int, String] =
    if buffer.hidden || buffer.annotations.notes.isEmpty then Map.empty
    else
      val noteContents = noteContentsOf(buffer, buffers)
      entries.get().get(buffer.id).filter(isFresh(_, buffer, noteContents)) match
        case Some(entry) => entry.ghosts
        case None =>
          val overviews = overviewsOf(buffer, buffers, entries.get().get(buffer.id).map(_.overviews))
          val ghosts    = ChapterGhosts.byLine(buffer, buffers, note => overviews.get(note.id).fold(Nil)(_._2))
          val _ = entries.updateAndGet(
            _.updated(
              buffer.id,
              Entry(buffer.document.content, buffer.annotations.notes, noteContents, overviews, ghosts)
            )
          )
          ghosts

  /** Each chapter note's overview lines, read again only for a note whose text is not the one `previous` read, so
    * typing in the manuscript does not copy every note.
    */
  private def overviewsOf(
    buffer: Buffer,
    buffers: Map[BufferId, Buffer],
    previous: Option[Map[BufferId, (Rope, List[String])]]
  ): Map[BufferId, (Rope, List[String])] =
    buffer.annotations.notes.toList
      .collect { case (NoteKey.Chapter(_), notes) => notes.overview }
      .flatMap(buffers.get)
      .map { note =>
        val content = note.document.content
        val lines = previous
          .flatMap(_.get(note.id))
          .collect { case (read, lines) if sameObject(read, content) => lines }
          .getOrElse(ChapterGhosts.overviewLines(note))
        note.id -> (content, lines)
      }
      .toMap

  private def noteContentsOf(buffer: Buffer, buffers: Map[BufferId, Buffer]): Map[BufferId, Rope] =
    buffer.annotations.notes.values
      .flatMap(_.bufferIds)
      .flatMap(id => buffers.get(id).map(note => id -> note.document.content))
      .toMap

  private def isFresh(entry: Entry, buffer: Buffer, noteContents: Map[BufferId, Rope]): Boolean =
    sameObject(entry.content, buffer.document.content) &&
      entry.notes == buffer.annotations.notes &&
      entry.noteContents.keySet == noteContents.keySet &&
      entry.noteContents.forall { case (id, content) => noteContents.get(id).exists(sameObject(content, _)) }

  private def sameObject(a: Rope, b: Rope): Boolean = (a: AnyRef) eq (b: AnyRef)
