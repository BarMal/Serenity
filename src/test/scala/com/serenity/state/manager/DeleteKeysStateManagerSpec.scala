package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{DeleteBackward, ExtendSelectionRight}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.{StateManagerTestSupport, setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Backspace through a running editor after Shift+Right had nowhere to go: the edit must land and dirty the buffer. */
class DeleteKeysStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Backspace after Shift+Right at the end of the document" should "delete the last character and dirty the buffer" in {
    val editor   = createStateManager("DeleteKeysStateManagerSpec")
    val bufferId = editor.createBuffer("hello", None).unsafeRunSync()
    editor.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    editor.setCursorPosition(PaneId(0), 0, 5).unsafeRunSync()

    editor.applyEvent(ExtendSelectionRight).unsafeRunSync()
    editor.applyEvent(DeleteBackward).unsafeRunSync()

    val state = editor.getCurrentState.unsafeRunSync()
    state.persisted.buffers(bufferId).document.content.collect() shouldBe "hell"
    state.persisted.buffers(bufferId).document.isDirty shouldBe true
  }
