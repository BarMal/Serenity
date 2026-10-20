package com.serenity.perf

import java.awt.image.BufferedImage

import cats.effect.unsafe.IORuntime
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameCounter
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.manager.{DamageProducer, StateManager}
import com.serenity.state.models.{Damage, HeadingIdentity, NoteKey, Notes}

/** A typing frame in an e-reader pane: 40-cell columns, each with its own line-number rail (#1863). The keystroke
  * reflows its paragraph, which wraps into some sixteen rows across the columns; every other row, and its rail band, is
  * kept. The same frame with chapter ghosts on, in the e-reader pane and in a single-column one, where the ghost rows
  * are drawn too.
  */
private[perf] object EreaderTypingBenchmarks:

  given Balance = Balance.default

  private val MaxRowsDrawn = 20L

  /** The paragraph [[LaptopFrameBenchmarks.proseStateManager]] types into; the ghosted manuscripts type into the same
    * one, since paragraphs differ in length and so in how many rows a keystroke reflows.
    */
  private val ProseTypingLine = 150

  private val config = AppConfig.conventional.withColumnMode(true).withColumnTargetWidth(40).withLineNumbers(true)

  private val singleColumnConfig = AppConfig.conventional

  /** A manuscript whose first chapter is empty under a chapter note, so its blank lines carry the note's ghost, ahead
    * of the prose typed into.
    */
  private def ghostedStateManager(config: AppConfig)(using IORuntime) =
    val chapterOpening = "# Chapter 1: Storm\n\n\n\n"
    val manuscript     = chapterOpening + BenchmarkFixtures.loremIpsumProse(300)
    val (stateManager, manuscriptId) =
      LaptopFrameBenchmarks.stateManagerHolding(
        manuscript,
        config,
        cursorLine = ProseTypingLine + chapterOpening.count(_ == '\n'),
        cursorColumn = 40
      )
    val noteId = stateManager.createBuffer("Gale hits\nLiz leaves", None).unsafeRunSync()
    stateManager
      .updateState { state =>
        val owner = state.persisted.buffers(manuscriptId)
        val annotated = owner.copy(
          document = owner.document.copy(language = Some(LanguageId.Markdown)),
          annotations =
            owner.annotations.copy(notes = Map(NoteKey.Chapter(HeadingIdentity("storm", 0)) -> Notes(noteId)))
        )
        state.copy(
          persisted = state.persisted.copy(buffers =
            state.persisted.buffers
              .updated(manuscriptId, annotated)
              .updated(noteId, state.persisted.buffers(noteId).copy(hidden = true))
          ),
          runtime = state.runtime.copy(chapterGhostsVisible = true)
        )
      }
      .unsafeRunSync()
    stateManager

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    typingBenchmark(
      "laptop.render.typing_frame_2x.ereader_railed",
      LaptopFrameBenchmarks.proseStateManager(config = config)._1
    ) ::
      typingBenchmark("laptop.render.typing_frame_2x.ereader_railed_ghosts", ghostedStateManager(config)) ::
      typingBenchmark("laptop.render.typing_frame_2x.chapter_ghosts", ghostedStateManager(singleColumnConfig)) :: Nil

  private def typingBenchmark(name: String, stateManager: StateManager)(using
    IORuntime
  ): BenchmarkRunner.Benchmark =
    val (before, after) = TypingTransition.of(stateManager)
    val forward         = DamageProducer.forTransition(before, after)
    val backward        = DamageProducer.forTransition(after, before)
    val frames          = LaptopFrameBenchmarks.PersistingFrames()
    val steps           = Iterator.continually(List(after -> forward, before -> backward)).flatten
    val _               = frames.render(before, Damage.Everything)

    def next(): BufferedImage =
      val (state, damage) = steps.next()
      frames.render(state, damage)

    def rowsDrawn(): Long =
      frames.counting {
        val _ = next()
        frames.lastFrame(FrameCounter.RowsDrawn)
      }

    BenchmarkRunner.Benchmark(
      name,
      2,
      12,
      () =>
        assert(forward != Damage.Everything, "typing damaged the whole frame")
        assert(LaptopFrameBenchmarks.hasPixels(next()), "typing frame drew nothing")
        val drawn = rowsDrawn()
        assert(drawn <= MaxRowsDrawn, s"the typing frame redrew $drawn rows, more than the $MaxRowsDrawn allowed")
      ,
      () => next()
    )
