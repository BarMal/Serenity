package com.serenity.command

import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.fonts.FontSizes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2405: the three font sizes as one value, a setting that changes them all, and one level of undo that remembers the
  * sizes from before the last change. Recording belongs to the live settings only: a preset being edited keeps none.
  */
class FontSizeUndoSpec extends AnyFlatSpec with Matchers:

  private val base = FontConfig(fontSize = 12.0f, textFontSize = 36.0f, uiFontSize = 11.0f)
  private val was  = FontSizes(prose = 36.0f, code = 12.0f, ui = 11.0f)

  private def live(intent: FontIntent, from: FontConfig = base): FontConfig = FontConfigEdit.recording(intent)(from)

  "A font config's sizes" should "be its prose, code and UI sizes" in {
    base.sizes shouldBe was
    base.withSizes(FontSizes(1.0f + 8.0f, 20.0f, 30.0f)).sizes shouldBe FontSizes(9.0f, 20.0f, 30.0f)
  }

  "Setting all sizes" should "set prose, code and UI to one size" in {
    FontConfigEdit(FontIntent.SetAllFontSizes(16.0f))(base).sizes shouldBe FontSizes(16.0f, 16.0f, 16.0f)
  }

  it should "clamp to the range every size input accepts" in {
    FontConfigEdit(FontIntent.SetAllFontSizes(500.0f))(base).sizes shouldBe FontSizes(48.0f, 48.0f, 48.0f)
    FontConfigEdit(FontIntent.SetAllFontSizes(1.0f))(base).sizes shouldBe FontSizes(8.0f, 8.0f, 8.0f)
  }

  "Recording a live change" should "keep the sizes from before it" in {
    val changed = live(FontIntent.SetTextFontSize(20.0f))

    changed.textFontSize shouldBe 20.0f
    changed.previousSizes shouldBe Some(was)
  }

  it should "record every command that changes a size" in {
    val intents = List(
      FontIntent.SetCodeFontSize(14.0f),
      FontIntent.SetTextFontSize(14.0f),
      FontIntent.SetUiFontSize(14.0f),
      FontIntent.SetFontSize(14.0f),
      FontIntent.IncreaseFontSize,
      FontIntent.DecreaseFontSize,
      FontIntent.SetAllFontSizes(14.0f)
    )

    intents.map(intent => intent -> live(intent).previousSizes) shouldBe intents.map(_ -> Some(was))
  }

  it should "record nothing when the sizes stay as they were" in {
    live(FontIntent.SetTextFontSize(36.0f)).previousSizes shouldBe None
    live(FontIntent.SetCodeFontFamily("Monospaced")).previousSizes shouldBe None
    live(FontIntent.SetLigatures(false)).previousSizes shouldBe None
  }

  it should "leave what was recorded alone when a later change is not a size" in {
    val changed = live(FontIntent.SetTextFontSize(20.0f))

    live(FontIntent.SetTextFontFamily("Serif"), changed).previousSizes shouldBe Some(was)
  }

  it should "keep only the sizes from before the last change, not the first" in {
    val first  = live(FontIntent.SetTextFontSize(20.0f))
    val second = live(FontIntent.SetUiFontSize(15.0f), first)

    second.previousSizes shouldBe Some(first.sizes)
  }

  "A preset's edit of a size" should "record nothing, since a preset is not undone" in {
    FontConfigEdit(FontIntent.SetTextFontSize(20.0f))(base).previousSizes shouldBe None
  }

  "Undoing the last size change" should "restore exactly the sizes from before it, and forget them" in {
    val changed = live(FontIntent.SetAllFontSizes(16.0f))

    val undone = live(FontIntent.UndoFontSizeChange, changed)

    undone.sizes shouldBe was
    undone.previousSizes shouldBe None
  }

  it should "leave families, scale and ligatures as they were" in {
    val changed = live(FontIntent.SetAllFontSizes(16.0f)).copy(textFontFamily = "Serif", textScaleMultiplier = 1.5)

    val undone = live(FontIntent.UndoFontSizeChange, changed)

    undone.copy(previousSizes = None) shouldBe base.copy(textFontFamily = "Serif", textScaleMultiplier = 1.5)
  }

  it should "do nothing a second time, and nothing when there was no change" in {
    val undone = live(FontIntent.UndoFontSizeChange, live(FontIntent.SetAllFontSizes(16.0f)))

    live(FontIntent.UndoFontSizeChange, undone) shouldBe undone
    live(FontIntent.UndoFontSizeChange) shouldBe base
  }
