package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.text.TextEncoding
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1627: a save that had to fall back to UTF-8 rewrote the file in UTF-8, so the buffer in the state must say so too,
  * or the next save would try the old encoding again.
  */
class SavedEncodingMergeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val path     = Path.of("/workspace/notes.txt")

  private def stateWith(buffer: Buffer): AppState =
    val initial = AppState.initial
    initial.copy(persisted = initial.persisted.copy(buffers = initial.persisted.buffers.updated(bufferId, buffer)))

  "Merging a save result" should "take the encoding and BOM the file was actually written with" in {
    val latin1 = Buffer.fromString(bufferId, "café 🙂\n")
    val opened = latin1.copy(document =
      latin1.document.copy(filePath = Some(path), encoding = TextEncoding.Windows1252, hasBom = false)
    )
    val written = opened.copy(document = opened.document.copy(encoding = TextEncoding.Utf8, hasBom = false))
    val save    = FileSave(bufferId, path, opened, SaveKind.Save, writtenAtSubmit = 0L)

    val merged = FileResults.saved(stateWith(opened), save, written).persisted.buffers(bufferId).document

    (merged.encoding, merged.hasBom) shouldBe ((TextEncoding.Utf8, false))
  }
end SavedEncodingMergeSpec
