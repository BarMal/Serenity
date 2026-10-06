package com.serenity.state.models

import com.serenity.config.StatusSegment
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.LspProgressTask
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StatusLineTextSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def stateWith(text: String, wordGoal: Option[Int]): AppState =
    val buffer = Buffer.fromString(bufferId, text)
    val base   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    base.copy(persisted = base.persisted.copy(config = base.persisted.config.withWordGoal(wordGoal)))

  "WordGoal" should "report no goal when none is configured" in {
    val state = stateWith("one two three", wordGoal = None)

    StatusLineText.render(state, List(StatusSegment.WordGoal)) shouldBe Some("No word goal set")
  }

  it should "show progress toward the configured goal" in {
    val state = stateWith("one two three", wordGoal = Some(10))

    StatusLineText.render(state, List(StatusSegment.WordGoal)) shouldBe Some("3 / 10 words (30%)")
  }

  it should "cap displayed progress at 100% once the goal is met or exceeded" in {
    val state = stateWith("one two three four five", wordGoal = Some(3))

    StatusLineText.render(state, List(StatusSegment.WordGoal)) shouldBe Some("5 / 3 words (100%)")
  }

  it should "count a spaceless Chinese sentence one word per character toward the goal" in {
    val state = stateWith("我们今天去公园", wordGoal = Some(10))

    StatusLineText.render(state, List(StatusSegment.WordGoal)) shouldBe Some("7 / 10 words (70%)")
  }

  "Language" should "show the work its language server reports as running" in {
    val scalaBuffer = Buffer.fromString(bufferId, "object A")
    val withLanguage =
      scalaBuffer.copy(document = scalaBuffer.document.copy(language = Some(LanguageId.Scala)))
    val base =
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> withLanguage)))
    val indexing = LspProgressTask("index", "Indexing", None, Some(40))
    val state = base.copy(runtime =
      base.runtime.copy(languageService =
        base.runtime.languageService.copy(progress = Map(LanguageId.Scala -> List(indexing)))
      )
    )

    StatusLineText.render(base, List(StatusSegment.Language)) shouldBe Some("Scala")
    StatusLineText.render(state, List(StatusSegment.Language)) shouldBe Some("Scala (Indexing 40%)")
  }
