package com.serenity.state.models

import com.serenity.testkit.Generators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `List[CursorPosition].primaryCursor` (`#1066`) centralises the `cursors.headOption.getOrElse(CursorPosition(0, 0))`
  * fallback repeated across `EditorEventReducer` and `StateManagerViewportCapability`.
  */
class CursorPositionPrimaryCursorSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  property("defaults to the document origin for an empty cursor list") {
    List.empty[CursorPosition].primaryCursor shouldBe CursorPosition(0, 0)
  }

  property("returns the single cursor for a one-element list") {
    forAll(Generators.genCursorPosition)(cursor => List(cursor).primaryCursor shouldBe cursor)
  }

  property("returns the head, not any other cursor, for a multi-cursor list") {
    forAll(Generators.genCursorPosition, Generators.genCursorPosition, Generators.genCursorPosition) {
      (first, second, third) => List(first, second, third).primaryCursor shouldBe first
    }
  }
