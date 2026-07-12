package com.serenity

import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReplaceWorkflowScopeSpec extends AnyFlatSpec with Matchers:

  "ReplaceWorkflowScope" should "resolve whole-buffer replacement without selection bounds" in {
    ReplaceWorkflowScope.CurrentBuffer.resolve(None, _ => fail("whole-buffer scope does not need offsets")) shouldBe
      Right(ReplaceScopeRange.WholeBuffer)
  }

  it should "resolve a selection to ordered replacement bounds" in {
    val selection = Selection(CursorPosition(2, 4), CursorPosition(1, 3))

    ReplaceWorkflowScope.Selection.resolve(
      Some(selection),
      cursor => if cursor == selection.start then 10 else 20
    ) shouldBe Right(ReplaceScopeRange.Selection(10, 20))
  }

  it should "report a typed error when selection replacement has no selection" in {
    ReplaceWorkflowScope.Selection.resolve(None, _ => 0) shouldBe Left(ReplaceScopeError.MissingSelection)
  }
end ReplaceWorkflowScopeSpec
