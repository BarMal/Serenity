package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EmptyBufferDisplaySpec extends AnyFunSpec with Matchers:
  given Balance = Balance.default

  describe("Empty buffer display behavior"):
    it("should show welcome text for new empty buffers"):
      val bufferId = BufferId(1)
      val buffer   = Buffer.newEmpty(bufferId)

      buffer.document.content.weight shouldEqual 0
      buffer.document.isNewEmpty shouldEqual true

      // This represents the renderer's decision logic
      val shouldShowWelcome = buffer.document.content.weight == 0 && buffer.document.isNewEmpty
      shouldShowWelcome shouldEqual true

    it("should show ~Empty~ for used empty buffers"):
      val bufferId  = BufferId(1)
      val newBuffer = Buffer.newEmpty(bufferId)
      val buffer    = newBuffer.copy(document = newBuffer.document.copy(isNewEmpty = false))

      buffer.document.content.weight shouldEqual 0
      buffer.document.isNewEmpty shouldEqual false

      // This represents the renderer's decision logic
      val shouldShowEmpty = buffer.document.content.weight == 0 && !buffer.document.isNewEmpty
      shouldShowEmpty shouldEqual true

    it("should show content for non-empty buffers"):
      val bufferId = BufferId(1)
      val buffer   = Buffer.fromString(bufferId, "hello world")

      buffer.document.content.weight should be > 0
      buffer.document.content.collect() shouldEqual "hello world"

    it("should transition from welcome text to content when typing"):
      val bufferId      = BufferId(1)
      val newBuffer     = Buffer.newEmpty(bufferId)
      val initialBuffer = newBuffer.copy(editing = newBuffer.editing.copy(cursors = List(CursorPosition(0, 0))))
      val paneId        = PaneId(1)
      val pane          = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)

      val initialState = AppState(
        buffers = Map(bufferId -> initialBuffer),
        layout = Layout(Map(paneId -> pane), Some(paneId)),
        focus = Focus.EditorPane(paneId)
      )

      val component = EditorPaneComponent(paneId)

      // Initial state should show welcome text
      initialBuffer.document.isNewEmpty shouldEqual true
      initialBuffer.document.content.weight shouldEqual 0

      // Type a character
      val result = component.processEvent(InsertChar('h'), initialState)
      result match
        case ComponentResult.ReducerUpdate(reducerResult) =>
          val newState      = reducerResult.state
          val updatedBuffer = newState.buffers(bufferId)

          // After typing, should no longer be new empty
          updatedBuffer.document.isNewEmpty shouldEqual false
          updatedBuffer.document.content.weight should be > 0
          updatedBuffer.document.content.collect() shouldEqual "h"
        case _ => fail("Expected state change")

    it("should transition from welcome text to ~Empty~ when deleting all content"):
      val bufferId     = BufferId(1)
      val stringBuffer = Buffer.fromString(bufferId, "h")
      val bufferWithContent =
        stringBuffer.copy(
          document = stringBuffer.document.copy(isNewEmpty = false),
          editing = stringBuffer.editing.copy(cursors = List(CursorPosition(0, 1)))
        )
      val paneId = PaneId(1)
      val pane   = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)

      val initialState = AppState(
        buffers = Map(bufferId -> bufferWithContent),
        layout = Layout(Map(paneId -> pane), Some(paneId)),
        focus = Focus.EditorPane(paneId)
      )

      val component = EditorPaneComponent(paneId)

      // Delete the character
      val result = component.processEvent(DeleteBackward, initialState)
      result match
        case ComponentResult.ReducerUpdate(reducerResult) =>
          val newState      = reducerResult.state
          val updatedBuffer = newState.buffers(bufferId)

          // Should be empty but not new empty (shows ~Empty~)
          updatedBuffer.document.content.weight shouldEqual 0
          updatedBuffer.document.isNewEmpty shouldEqual false
        case _ => fail("Expected state change")

    it("should handle newlines correctly in content weight"):
      val bufferId = BufferId(1)
      val buffer   = Buffer.fromString(bufferId, "\n")

      // Single newline should count as weight > 0
      buffer.document.content.weight shouldEqual 1
      buffer.document.content.collect() shouldEqual "\n"

    it("should handle multiple newlines correctly"):
      val bufferId = BufferId(1)
      val buffer   = Buffer.fromString(bufferId, "\n\n\n")

      // Multiple newlines should count as weight > 0
      buffer.document.content.weight shouldEqual 3
      buffer.document.content.collect() shouldEqual "\n\n\n"
