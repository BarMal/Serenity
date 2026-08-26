package com.serenity.state.models

import com.serenity.rope.Balance
import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `Buffer.cursorList`/`withCursorList` round-trip a buffer's cursor state through the uniform `Cursor` shape -- see
  * that method's doc comment for why the conversion exists instead of `Buffer` storing `Cursor`s directly.
  */
class CursorListSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:
  private given Balance = Balance.default

  private def emptyBuffer: Buffer = Buffer.empty(BufferId(0))

  private val genSingleCursorBuffer: Gen[Buffer] =
    for
      cursor          <- Generators.genCursorPosition
      selection       <- Gen.option(Generators.genCursorPosition)
      preferredColumn <- Gen.option(Gen.chooseNum(0, 500))
      preferredXPx    <- Gen.option(Gen.chooseNum(0f, 2000f))
    yield emptyBuffer.copy(
      cursors = List(cursor),
      selection = selection.map(anchor => Selection(anchor, cursor)),
      preferredColumn = if selection.isDefined then None else preferredColumn,
      preferredXPx = if selection.isDefined then None else preferredXPx
    )

  private val genMultiCursorBuffer: Gen[Buffer] =
    for
      cursors           <- Gen.listOfN(3, Generators.genCursorPosition)
      withVerticalState <- Gen.oneOf(true, false)
      xPx               <- Gen.chooseNum(0f, 2000f)
    yield
      val states =
        if withVerticalState then cursors.map(c => VerticalCursorState(c, c.column, xPx)) else Nil
      emptyBuffer.copy(cursors = cursors, multiCursorVerticalStates = states)

  private val genMultiSelectionBuffer: Gen[Buffer] =
    Gen.listOfN(3, Generators.genCursorPosition).map { cursors =>
      val selections = cursors.map(c => Selection(c, c))
      emptyBuffer.copy(cursors = cursors, selections = selections)
    }

  private val genCanonicalBuffer: Gen[Buffer] =
    Gen.oneOf(genSingleCursorBuffer, genMultiCursorBuffer, genMultiSelectionBuffer)

  property("withCursorList(cursorList(buffer)) reproduces the buffer's cursor-shaped fields") {
    forAll(genCanonicalBuffer) { buffer =>
      val roundTripped = buffer.withCursorList(buffer.cursorList)
      roundTripped.cursors shouldBe buffer.cursors
      roundTripped.selection shouldBe buffer.selection
      roundTripped.selections shouldBe buffer.selections
      roundTripped.preferredColumn shouldBe buffer.preferredColumn
      roundTripped.preferredXPx shouldBe buffer.preferredXPx
      roundTripped.multiCursorVerticalStates shouldBe buffer.multiCursorVerticalStates
    }
  }

  property("cursorList(withCursorList(buffer, cursors)) reproduces the cursor list") {
    forAll(genCanonicalBuffer) { buffer =>
      val cursors = buffer.cursorList
      emptyBuffer.withCursorList(cursors).cursorList shouldBe cursors
    }
  }

  property("a single cursor's selection round-trips through Cursor.selection") {
    forAll(Generators.genCursorPosition, Generators.genCursorPosition) { (anchor, focus) =>
      val cursor = Cursor(Selection(anchor, focus))
      cursor.selection shouldBe Some(Selection(anchor, focus))
      cursor.position shouldBe focus
    }
  }
