package com.serenity.ui.tui

import java.nio.file.Paths

import scala.collection.mutable.ListBuffer

import cats.effect.unsafe.implicits.global
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilitySync, TuiAccessibilityBridge}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TuiWindowTitleSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val esc = 0x1b.toChar.toString
  private val bel = 0x07.toChar.toString

  private def stateShowing(document: Document): AppState =
    val buffer = Buffer(BufferId(7), document)
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  private def document(text: String, fileName: Option[String] = Some("notes.md"), dirty: Boolean = false): Document =
    Document(Rope(text), filePath = fileName.map(Paths.get(_)), isDirty = dirty)

  "TuiWindowTitle" should "show the file name and the application name for a clean file" in {
    TuiWindowTitle.from(stateShowing(document("alpha"))) shouldBe "notes.md — Serenity"
  }

  it should "mark a dirty file with an asterisk after its name" in {
    TuiWindowTitle.from(stateShowing(document("alpha", dirty = true))) shouldBe "notes.md* — Serenity"
  }

  it should "name an untitled buffer by its display name" in {
    TuiWindowTitle.from(stateShowing(document("alpha", fileName = None))) shouldBe "Buffer 7 — Serenity"
    TuiWindowTitle.from(stateShowing(document("alpha", fileName = None, dirty = true))) shouldBe
      "Buffer 7* — Serenity"
  }

  it should "fall back to the application name when no buffer is active" in {
    val noBuffers = stateShowing(document("alpha"))
      .copy(persisted = stateShowing(document("alpha")).persisted.copy(buffers = Map.empty, bufferOrder = Nil))
    TuiWindowTitle.from(noBuffers) shouldBe "Serenity"
  }

  it should "never contain document text, even when a one-line document reads like a title" in {
    val text = "Chapter One: The Beginning"

    TuiWindowTitle.from(stateShowing(document(text, fileName = None, dirty = true))) should not include "Chapter"
    TuiWindowTitle.from(stateShowing(document(text))) should not include "Chapter"
  }

  "Publishing the TUI title" should "write it only when the name or dirty state changes, not on every edit" in {
    val written = ListBuffer.empty[String]
    val bridge  = new TuiAccessibilityBridge(text => if text.startsWith(s"$esc]0;") then written += text)
    val sync    = AccessibilitySync.empty.unsafeRunSync()
    val size    = ViewportSize(100, 30)

    def publish(state: AppState): Unit =
      TuiRuntime.syncAccessibility(state, size, sync, bridge).unsafeRunSync()

    publish(stateShowing(document("a")))
    publish(stateShowing(document("ab")))
    written.toList shouldBe List(s"$esc]0;notes.md — Serenity$bel")

    publish(stateShowing(document("abc", dirty = true)))
    publish(stateShowing(document("abcd", dirty = true)))
    publish(stateShowing(document("abcde", dirty = true)))
    written.toList shouldBe List(s"$esc]0;notes.md — Serenity$bel", s"$esc]0;notes.md* — Serenity$bel")

    publish(stateShowing(document("abcde", fileName = Some("other.md"), dirty = true)))
    written.toList.last shouldBe s"$esc]0;other.md* — Serenity$bel"
    written.size shouldBe 3
  }
