package com.serenity.state.models

import cats.data.NonEmptyList
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

  /** `Col` is the visual column: tab-expanded, with a wide character or an emoji cluster counted at its cells (#1967).
    */
  private def positionAt(text: String, line: Int, column: Int): Option[String] =
    val buffer = Buffer.fromString(bufferId, text)
    val placed = buffer.withCursorList(NonEmptyList.one(Cursor(CursorPosition(line, column))))
    val state  = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> placed)))
    StatusLineText.render(state, List(StatusSegment.Position))

  "Position" should "count a plain line's columns as before" in {
    positionAt("hello", 0, 5) shouldBe Some("Line 1, Col 6")
  }

  it should "count a flag, four UTF-16 units, as the two cells it takes" in {
    positionAt("\ud83c\uddfa\ud83c\uddf8", 0, 4) shouldBe Some("Line 1, Col 3")
  }

  it should "count a base and its combining mark as the one cell they take" in {
    positionAt("e\u0301x", 0, 3) shouldBe Some("Line 1, Col 3")
  }

  it should "count a joined family as the two cells it takes" in {
    positionAt("\ud83d\udc68\u200d\ud83d\udc69\u200d\ud83d\udc67x", 0, 9) shouldBe Some("Line 1, Col 4")
  }

  it should "count a wide CJK character as two cells" in {
    positionAt("\u4f60\u597d", 0, 2) shouldBe Some("Line 1, Col 5")
  }

  it should "expand a tab to the next tab stop" in {
    val tab = IndentationSettings.DefaultTabSize
    positionAt("\tx", 0, 2) shouldBe Some(s"Line 1, Col ${tab + 2}")
    positionAt("ab\tc", 0, 4) shouldBe Some(s"Line 1, Col ${tab + 2}")
  }

  it should "read the line the cursor is on" in {
    positionAt("one\n\u4f60\u597d", 1, 1) shouldBe Some("Line 2, Col 3")
  }
