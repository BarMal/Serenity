package com.serenity.state.models

import com.serenity.richtext.RichTextStyle
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppStateEqualitySpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "AppState" should "compare equal to itself via reference identity" in {
    val state = AppState.initial
    (state == state) shouldBe true
  }

  it should "compare equal to a structurally identical but separately constructed instance" in {
    AppState.initial shouldBe AppState.initial
  }

  it should "detect a difference confined to its last declared field (windowSitter)" in {
    val base    = AppState.initial
    val changed = base.copy(windowSitter = base.windowSitter.copy(activeTicks = 1))

    (base == changed) shouldBe false
  }

  it should "detect a difference in an early field (focus)" in {
    val base    = AppState.initial
    val changed = base.copy(focus = Focus.Surface(SurfaceId("changed")))

    (base == changed) shouldBe false
  }

  it should "still compare buffers structurally through the buffers map" in {
    val bufferId         = BufferId(1)
    val base             = AppState.initial.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello")))
    val sameContent      = AppState.initial.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello")))
    val differentContent = AppState.initial.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "world")))

    (base == sameContent) shouldBe true
    (base == differentContent) shouldBe false
  }

  "Buffer" should "compare equal to itself via reference identity" in {
    val buffer = Buffer.fromString(BufferId(1), "hello")
    (buffer == buffer) shouldBe true
  }

  it should "compare equal to a structurally identical but separately constructed instance" in {
    Buffer.fromString(BufferId(1), "hello") shouldBe Buffer.fromString(BufferId(1), "hello")
  }

  it should "detect a difference confined to its last declared field (insertionRichTextStyle)" in {
    val base    = Buffer.fromString(BufferId(1), "hello")
    val changed = base.copy(richText = base.richText.copy(insertionRichTextStyle = Some(RichTextStyle())))

    (base == changed) shouldBe false
  }

  it should "detect a difference in content" in {
    val base    = Buffer.fromString(BufferId(1), "hello")
    val changed = Buffer.fromString(BufferId(1), "world")

    (base == changed) shouldBe false
  }

  it should "detect a difference confined to its new last declared field (markdownPreviewCommittedGeneration)" in {
    val base    = Buffer.fromString(BufferId(1), "hello")
    val changed = base.copy(markdownPreviewCommittedGeneration = 1L)

    (base == changed) shouldBe false
  }
