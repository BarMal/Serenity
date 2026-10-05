package com.serenity.state.manager

import com.serenity.config.{StatusLinePlacement, StatusSegment}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1891: damage stays bounded by what changed on screen, whatever the buffer's size, and the status row is only
  * damaged when the text it shows changes.
  */
class DamageProducerScreenRowsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private val hundredThousandLines = (1 to 100_000).map(line => s"line $line").mkString("\n")

  private def stateWithContent(text: String, cursor: CursorPosition = CursorPosition(0, 0)): AppState =
    withBuffer(AppState.initial)(buffer =>
      buffer.copy(document = buffer.document.copy(content = Rope(text)), editing = EditingState(List(cursor)))
    )

  private def withBuffer(state: AppState)(update: Buffer => Buffer): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, update(state.persisted.buffers(bufferId)))
      )
    )

  private def withConfig(state: AppState)(update: com.serenity.config.AppConfig => com.serenity.config.AppConfig) =
    state.copy(persisted = state.persisted.copy(config = update(state.persisted.config)))

  "Viewport damage" should "be one bounded fact on a 100k-line buffer, with no status row damage" in {
    val before = stateWithContent(hundredThousandLines)
    val after  = withBuffer(before)(_.copy(viewport = Viewport.default.copy(topLine = 50_000)))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferAll(bufferId)
  }

  "Language damage" should "be one bounded fact on a 100k-line buffer" in {
    val before = stateWithContent(hundredThousandLines)
    val after =
      withBuffer(before)(buffer => buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Scala))))

    // The status row names the language, so it is damaged too.
    DamageProducer.forTransition(before, after) shouldBe Damage.Combined(Set(Damage.BufferAll(bufferId), Damage.Chrome))
  }

  "Focus-dimming damage" should "be one bounded fact when the caret leaves every block" in {
    val before = withConfig(stateWithContent(hundredThousandLines))(config =>
      config.copy(surfaceConfig = config.surfaceConfig.copy(focusedTextBodyEnabled = true))
    )
    val after = withBuffer(before)(_.copy(editing = EditingState(List(CursorPosition(200_000, 0)))))

    val damage = DamageProducer.forTransition(before, after)
    Damage.damagesEveryRow(bufferId, damage) shouldBe true
    Damage.coarsenToRows(bufferId, damage).size should be <= 2
  }

  "Caret damage" should "name the old and new caret cells, not their whole lines" in {
    val before = stateWithContent("a long paragraph that wraps", CursorPosition(0, 2))
    val after  = withBuffer(before)(_.copy(editing = EditingState(List(CursorPosition(0, 20)))))

    Damage.damagedSpans(bufferId, DamageProducer.forTransition(before, after)) shouldBe
      Set(Damage.BufferCells(bufferId, 0, 2, Some(3)), Damage.BufferCells(bufferId, 0, 20, Some(21)))
    Damage.damagedLines(bufferId, DamageProducer.forTransition(before, after)) shouldBe empty
  }

  "Status row damage" should "not be reported for a caret move when the status row does not show the position" in {
    val before = withConfig(stateWithContent("alpha\nbeta", CursorPosition(0, 0)))(config =>
      config.copy(statusLine = config.statusLine.copy(segments = List(StatusSegment.Title)))
    )
    val after = withBuffer(before)(_.copy(editing = EditingState(List(CursorPosition(1, 2)))))

    Damage.touchesChrome(DamageProducer.forTransition(before, after)) shouldBe false
  }

  it should "not be reported when the status line is not pinned" in {
    val before = withConfig(stateWithContent("alpha\nbeta", CursorPosition(0, 0)))(config =>
      config.copy(statusLine = config.statusLine.copy(placement = StatusLinePlacement.Off))
    )
    val after = withBuffer(before)(_.copy(editing = EditingState(List(CursorPosition(1, 2)))))

    Damage.touchesChrome(DamageProducer.forTransition(before, after)) shouldBe false
  }

  it should "be reported when typing changes a word count the status row shows" in {
    val before = withConfig(stateWithContent("alpha", CursorPosition(0, 5)))(config =>
      config.copy(statusLine = config.statusLine.copy(segments = List(StatusSegment.WordCount)))
    )
    val after = withBuffer(before) { buffer =>
      buffer.copy(document =
        buffer.document.copy(content = buffer.document.content.insert(5, " beta").getOrElse(fail("insert failed")))
      )
    }

    Damage.touchesChrome(DamageProducer.forTransition(before, after)) shouldBe true
  }
