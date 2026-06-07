package com.serenity

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextLayoutSnapshotProportionalSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val propFont = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  "TextLayoutSnapshot.isProportional" should "be false for a monospaced font" in {
    val buffer = Buffer.fromString(BufferId(0), "hello")
    val snap   = TextLayoutSnapshot.fromBuffer(buffer, 200, monoFont)
    snap.isProportional shouldBe false
  }

  it should "be true for a SANS_SERIF font" in {
    val buffer = Buffer.fromString(BufferId(0), "hello")
    val snap   = TextLayoutSnapshot.fromBuffer(buffer, 200, propFont)
    snap.isProportional shouldBe true
  }

  it should "be false for an empty buffer with a monospaced font" in {
    val buffer = Buffer.fromString(BufferId(0), "")
    val snap   = TextLayoutSnapshot.fromBuffer(buffer, 200, monoFont)
    snap.isProportional shouldBe false
  }
