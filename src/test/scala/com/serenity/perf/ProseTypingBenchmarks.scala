package com.serenity.perf

import java.util.concurrent.atomic.AtomicReference

import com.serenity.keystroke.events.{InsertChar, NewLine}
import com.serenity.lsp.config.LanguageId
import com.serenity.perf.BenchmarkFixtures.{editorState, largeRichTextDocument}
import com.serenity.rope.Balance
import com.serenity.state.core.NotesPaneSync
import com.serenity.state.manager.ChapterGhostCache
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer

/** Typing into the large prose documents the per-edit derived-data work (#1870, #1848) is measured on: a 20,000
  * paragraph rich-text buffer, and a 1,000 chapter Markdown buffer with a chapter note. Each keystroke is applied to
  * the state the previous one produced, as in a session, so caches that advance from the last edit are exercised.
  */
private[perf] object ProseTypingBenchmarks:

  given Balance = Balance.default

  private val RichParagraphs = 20_000
  private val RichLine       = 10_000
  private val Chapters       = 1_000
  private val NoteChapter    = 500

  // heading, blank, prose, blank, prose, then the blank line between chapters
  private val LinesPerChapter = 6
  private val ProseLine       = LinesPerChapter * (NoteChapter - 1) + 2

  private val BufferIdUnderEdit = BufferId(1)
  private val NoteBufferId      = BufferId(2)

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    List(richTextTyping(), markdownTypingWithGhosts(), markdownTypingWithNotesPane(), markdownEnter())

  /** Runs just these benchmarks, which need no window: `sbt "Test/runMain com.serenity.perf.ProseTypingBenchmarks"`. */
  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks()))

  private def withCursor(state: AppState, line: Int, column: Int): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updatedWith(BufferIdUnderEdit)(
          _.map(buffer => buffer.copy(editing = EditingState(List(CursorPosition(line, column)))))
        )
      )
    )

  private def richTextState: AppState =
    val document = largeRichTextDocument(RichParagraphs)
    val base     = editorState(document.plainText, None)
    withCursor(
      base.copy(persisted =
        base.persisted.copy(buffers =
          base.persisted.buffers.view
            .mapValues(buffer =>
              buffer.copy(richText = buffer.richText.withSyncedDocument(Some(document), buffer.document.contentVersion))
            )
            .toMap
        )
      ),
      RichLine,
      5
    )

  private def novel: String =
    (1 to Chapters)
      .map(n => s"# Chapter $n\n\nSome prose for chapter $n, enough to be a line.\n\nMore prose, still chapter $n.")
      .mkString("\n\n")

  /** Chapter [[NoteChapter]] has a note, as a writer using the notes pane would. */
  private def markdownState: AppState =
    val base  = editorState(novel, Some(LanguageId.Markdown))
    val noted = HeadingIdentity("", NoteChapter - 1)
    val note  = Buffer.fromString(NoteBufferId, "Overview of the chapter.").copy(hidden = true)
    val owner = base.persisted.buffers(BufferIdUnderEdit)
    val annotated =
      owner.copy(annotations = owner.annotations.copy(notes = Map(NoteKey.Chapter(noted) -> Notes(NoteBufferId))))
    val state = base.copy(persisted =
      base.persisted.copy(buffers =
        base.persisted.buffers.updated(BufferIdUnderEdit, annotated).updated(NoteBufferId, note)
      )
    )
    withCursor(state, ProseLine, 5)

  private val NotesPaneId = PaneId(1)

  /** [[markdownState]] with the notes pane open beside the manuscript, as a writer keeping chapter notes has it. */
  private def notesPaneState: AppState =
    val base = markdownState
    base.copy(
      persisted = base.persisted.copy(layout =
        base.persisted.layout.copy(editorPanes =
          base.persisted.layout.editorPanes.updated(NotesPaneId, EditorPane.withBuffer(NotesPaneId, NoteBufferId))
        )
      ),
      runtime = base.runtime.copy(notesPane = Some(NotesPane(NotesPaneId, PaneId(0))))
    )

  private def typed(state: AtomicReference[AppState]): AppState =
    state.updateAndGet(current => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), current).state)

  private def richTextTyping(): BenchmarkRunner.Benchmark =
    val state = new AtomicReference(richTextState)
    def verify(): Unit =
      val after  = typed(new AtomicReference(richTextState)).persisted.buffers(BufferIdUnderEdit)
      val inSync = after.richText.richTextDocument.filter(_ => after.richTextInSync)
      assert(inSync.flatMap(_.paragraphAt(RichLine)).exists(_.plainText.contains("x")))
    BenchmarkRunner.Benchmark(
      "prose.rich_text_typing",
      3,
      BenchmarkIterationCounts.Reducer,
      verify,
      () => typed(state),
      fixedSampleCount = true
    )

  private def markdownTypingWithGhosts(): BenchmarkRunner.Benchmark =
    val state = new AtomicReference(markdownState)
    val cache = new ChapterGhostCache
    def verify(): Unit =
      val after = typed(new AtomicReference(markdownState)).persisted.buffers(BufferIdUnderEdit)
      assert(after.document.content.getLine(ProseLine).exists(_.contains("x")))
    def run(): Any =
      val after = typed(state)
      cache.ghostsFor(after.persisted.buffers(BufferIdUnderEdit), after.persisted.buffers)
    BenchmarkRunner.Benchmark(
      "prose.markdown_typing.chapter_ghosts",
      3,
      BenchmarkIterationCounts.Reducer,
      verify,
      run,
      fixedSampleCount = true
    )

  private def markdownTypingWithNotesPane(): BenchmarkRunner.Benchmark =
    val state = new AtomicReference(notesPaneState)
    def verify(): Unit =
      val before = notesPaneState
      val after  = typed(new AtomicReference(before))
      assert(
        NotesPaneSync.synced(after, before).persisted.layout.editorPanes(NotesPaneId).bufferId.contains(NoteBufferId)
      )
    def run(): Any =
      val before = state.get()
      NotesPaneSync.synced(typed(state), before)
    BenchmarkRunner.Benchmark(
      "prose.markdown_typing.notes_pane",
      3,
      BenchmarkIterationCounts.Reducer,
      verify,
      run,
      fixedSampleCount = true
    )

  private def markdownEnter(): BenchmarkRunner.Benchmark =
    val state = new AtomicReference(markdownState)
    // A writer ends a paragraph with two Enters, which leaves a blank line between it and the next.
    def entered(current: AppState): AppState =
      List(NewLine, NewLine).foldLeft(current)((state, event) =>
        EditorEventReducer.reduce(event, PaneId(0), state).state
      )
    def verify(): Unit =
      val after = entered(markdownState).persisted.buffers(BufferIdUnderEdit)
      assert(after.document.content.lineCount == LinesPerChapter * Chapters + 1)
    BenchmarkRunner.Benchmark(
      "prose.markdown_enter.renumber",
      3,
      BenchmarkIterationCounts.Reducer,
      verify,
      () => state.updateAndGet(entered),
      fixedSampleCount = true
    )

end ProseTypingBenchmarks
