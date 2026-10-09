package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.rope.{Balance, Leaf, Node, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A save is clean when the text is still the text that was written, however the rope holding it was shaped (#1942). */
class SavedAfterRebuildSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val path     = Path.of("/workspace/notes.txt")

  private def bufferHolding(content: Rope, dirty: Boolean): Buffer =
    val base = Buffer.fromString(bufferId, "")
    base.copy(document = base.document.withContent(content).copy(filePath = Some(path), isDirty = dirty))

  private def stateWith(buffer: Buffer): AppState =
    val initial = AppState.initial
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> buffer), bufferOrder = List(bufferId)))

  private val written = bufferHolding(Leaf("alpha"), dirty = true)
  private val save    = FileSave(bufferId, path, written, SaveKind.Save, writtenAtSubmit = 0L)

  "A save whose text the buffer still holds in a different rope shape" should "leave the buffer clean" in {
    val reshaped = bufferHolding(Node(Leaf("al"), Leaf("pha")), dirty = true)

    val merged = FileResults.saved(stateWith(reshaped), save, written).persisted.buffers(bufferId).document

    merged.isDirty shouldBe false
  }

  it should "not rewrite the content or bump its version" in {
    val reshaped = bufferHolding(Node(Leaf("al"), Leaf("pha")), dirty = true)

    val merged = FileResults.saved(stateWith(reshaped), save, written).persisted.buffers(bufferId).document

    merged.contentVersion shouldBe reshaped.document.contentVersion
    merged.content should be theSameInstanceAs reshaped.document.content
  }

  "A save whose text the buffer has since changed" should "leave the buffer dirty, whatever the rope shape" in {
    val typed = bufferHolding(Node(Leaf("al"), Leaf("pha!")), dirty = true)

    FileResults.saved(stateWith(typed), save, written).persisted.buffers(bufferId).document.isDirty shouldBe true
  }
end SavedAfterRebuildSpec
