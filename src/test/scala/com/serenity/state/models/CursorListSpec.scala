package com.serenity.state.models

import com.serenity.rope.Balance
import com.serenity.testkit.{EditingStateFixtures, Generators, VerticalCursorState}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `Buffer.cursorList`/`withCursorList` became trivial identity conversions once `EditingState` itself stores
  * cursors as `NonEmptyList[Cursor]` (`#1577`) -- this now exercises that identity directly, rather than the
  * five-field round trip `cursorList`/`withCursorList` used to perform before storage itself was per-cursor.
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
    yield emptyBuffer.copy(editing =
      EditingStateFixtures(
        cursors = List(cursor),
        selection = selection.map(anchor => Selection(anchor, cursor)),
        preferredColumn = if selection.isDefined then None else preferredColumn,
        preferredXPx = if selection.isDefined then None else preferredXPx
      )
    )

  private val genMultiCursorBuffer: Gen[Buffer] =
    for
      cursors           <- Gen.listOfN(3, Generators.genCursorPosition)
      withVerticalState <- Gen.oneOf(true, false)
      xPx               <- Gen.chooseNum(0f, 2000f)
    yield
      val states =
        if withVerticalState then cursors.map(c => VerticalCursorState(c, c.column, xPx)) else Nil
      emptyBuffer.copy(editing = EditingStateFixtures(cursors = cursors, multiCursorVerticalStates = states))

  private val genMultiSelectionBuffer: Gen[Buffer] =
    Gen.listOfN(3, Generators.genCursorPosition).map { cursors =>
      val selections = cursors.map(c => Selection(c, c))
      emptyBuffer.copy(editing = EditingStateFixtures(cursors = cursors, selections = selections))
    }

  private val genCanonicalBuffer: Gen[Buffer] =
    Gen.oneOf(genSingleCursorBuffer, genMultiCursorBuffer, genMultiSelectionBuffer)

  property("cursorList is exactly the buffer's own cursors") {
    forAll(genCanonicalBuffer) { buffer =>
      buffer.cursorList shouldBe buffer.editing.cursors
    }
  }

  property("withCursorList(cursorList(buffer)) reproduces the buffer's cursors") {
    forAll(genCanonicalBuffer) { buffer =>
      buffer.withCursorList(buffer.cursorList).editing.cursors shouldBe buffer.editing.cursors
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
