package com.serenity.perf

import scala.util.Random

import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.{AppState, Buffer, BufferId, EditingState}
import com.serenity.ui.layout.{PaneScroll, SplitAxis}

/** Open buffers that differ line by line, for the rows that ask how performance scales with them (#2478). The text
  * caches are keyed by text, not by buffer, so buffers that repeat each other's lines would share every entry and hide
  * the cost the rows are there to measure.
  */
private[perf] object MultiBufferFixture:

  given Balance = Balance.default

  enum Kind(val label: String):
    case Code  extends Kind("code")
    case Prose extends Kind("prose")

  /** `lines` code lines, or `lines / ProseLinesPerParagraph` prose paragraphs: about the same characters either way. */
  final case class Size(lines: Int)

  private val ProseLinesPerParagraph = 8

  /** The lines of the existing footprint rows, so the heap rows are comparable with them. */
  val FullSize: Size = Size(FootprintBenchmarks.DocumentLines)
  val TestSize: Size = Size(200)

  /** The largest prose buffer whose bar is counted late: one paragraph and one blank line each, just under the line
    * limit above which a bar is estimated at once (`PaneScroll.ExactLineLimit`).
    */
  val LateCountSize: Size = Size(((PaneScroll.ExactLineLimit + 1) / 2) * ProseLinesPerParagraph)

  private val WordsPerParagraph = 100

  def document(kind: Kind, index: Int, size: Size): String =
    kind match
      case Kind.Code  => (1 to size.lines).map(codeLine(index, _)).mkString("\n")
      case Kind.Prose => (1 to size.lines / ProseLinesPerParagraph).map(paragraph(index, _)).mkString("\n\n")

  private def randomFor(index: Int, n: Int): Random = new Random(index * 1_000_003L + n)

  private def codeLine(index: Int, n: Int): String =
    val token = randomFor(index, n).alphanumeric.take(24).mkString
    s"val item_${index}_$n = compute($index, $n, \"$token\") // line $n of buffer $index"

  private def paragraph(index: Int, n: Int): String =
    val random = randomFor(index, n)
    val words  = Vector.fill(WordsPerParagraph)(random.alphanumeric.take(3 + random.nextInt(7)).mkString.toLowerCase)
    words.mkString(" ").capitalize + "."

  /** `buffers` buffers of `kind`, in id order 1 to `buffers`, the first shown in the one pane. */
  def state(kind: Kind, buffers: Int, size: Size): AppState =
    val language = Option.when(kind == Kind.Code)(LanguageId.Scala)
    val base     = BenchmarkFixtures.editorState(document(kind, 1, size), language)
    val shown    = base.persisted.buffers(BufferId(1))
    val opened = (2 to buffers).map { index =>
      val id     = BufferId(index)
      val buffer = Buffer.fromString(id, document(kind, index, size))
      id -> buffer.copy(
        document = buffer.document.copy(language = language),
        viewport = shown.viewport,
        editing = EditingState(shown.editing.cursors)
      )
    }.toMap
    val all    = base.persisted.buffers ++ opened
    val config = if kind == Kind.Prose then AppConfig.conventional else base.persisted.config
    base.copy(
      persisted = base.persisted.copy(buffers = all, bufferOrder = all.keys.toList.sortBy(_.value), config = config),
      runtime = base.runtime.copy(nextBufferId = BufferId(buffers + 1))
    )

  /** `panes` panes side by side, the first showing the buffer it showed and each other showing the next buffer. */
  def split(state: AppState, panes: Int): AppState =
    (2 to panes).foldLeft(state) { (split, pane) =>
      val next = BufferId(pane)
      switchTo(EditorState.splitFocusedPane(split, SplitAxis.Vertical), next)
    }

  /** `buffer` in the active pane, as a tab click or `NextTab` puts it. */
  def switchTo(state: AppState, buffer: BufferId): AppState = EditorState.switchToBuffer(state, buffer)
