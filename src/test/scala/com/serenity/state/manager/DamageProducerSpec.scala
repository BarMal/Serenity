package com.serenity.state.manager

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.DirtyLineDiff
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DamageProducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def stateWithContent(text: String, cursors: List[CursorPosition] = List(CursorPosition(0, 0))): AppState =
    AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(content = Rope(text), cursors = cursors)
      )
    )

  "DamageProducer.forTransition" should "report no damage when nothing changed" in {
    val state = stateWithContent("alpha\nbeta\ngamma")
    DamageProducer.forTransition(state, state) shouldBe Damage.Nothing
  }

  it should "report no damage for a cursor-only move, since content is unchanged" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    val after = before.copy(buffers =
      before.buffers.updated(bufferId, before.buffers(bufferId).copy(cursors = List(CursorPosition(2, 3))))
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Nothing
  }

  it should "report the damaged row for a single-character edit on one line" in {
    val before       = stateWithContent("alpha\nbeta\ngamma")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(1, "X"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report every damaged row for an edit spanning a newline" in {
    val before       = stateWithContent("alpha\nbeta\ngamma")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(7, "X\nY"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    // "alpha\nbeX\nYta\ngamma" -- the insertion at offset 7 (into "beta", the second line) spans into a new line.
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "report the deletion's line even though the deleted range is empty in the result" in {
    val before       = stateWithContent("helloXworld")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.delete(5, 6))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report no damage for a buffer that did not exist before the transition" in {
    val before  = AppState.initial
    val otherId = BufferId(99)
    val after = before.copy(
      buffers = before.buffers.updated(otherId, before.buffers(bufferId).copy(id = otherId, content = Rope("new"))),
      bufferOrder = before.bufferOrder :+ otherId
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Nothing
  }

  it should "report Chrome damage when the theme changes" in {
    val before = stateWithContent("alpha")
    val after  = before.copy(theme = if before.theme == Theme.dark then Theme.light else Theme.dark)

    DamageProducer.forTransition(before, after) shouldBe Damage.Chrome
  }

  it should "combine content and chrome damage when both change together" in {
    val before       = stateWithContent("alpha\nbeta")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(0, "X"))
    val after = before
      .copy(buffers = before.buffers.updated(bufferId, editedBuffer))
      .copy(theme = if before.theme == Theme.dark then Theme.light else Theme.dark)

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0)), Damage.Chrome))
  }

  "DamageProducer's reported rows" should "cover what DirtyLineDiff independently finds dirty for the same edit" in {
    val before = stateWithContent("first line\nsecond line\nthird line\nfourth line")
    val buffer = before.buffers(bufferId)
    val edited = buffer.copy(content = buffer.content.insert(18, "-EDIT-"))
    val after  = before.copy(buffers = before.buffers.updated(bufferId, edited))

    val font           = com.serenity.ui.fonts.FontLoader.previewTextFont(after.config.fontConfig)
    val wrapPx         = com.serenity.ui.layout.TextLayoutSnapshot.gridWrapWidthPx(80, after.config.fontConfig)
    val beforeSnapshot = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)
    val afterSnapshot  = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(edited, wrapPx, font)
    val dirty          = DirtyLineDiff.dirtyRows(Some(beforeSnapshot), afterSnapshot)

    val damage = DamageProducer.forTransition(before, after)
    dirty.subsetOf(Damage.coarsenToRows(bufferId, damage)) shouldBe true
  }
