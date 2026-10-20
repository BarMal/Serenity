package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, HotkeyConfig}
import com.serenity.input.{FocusedInputTranslator, InputRouter}
import com.serenity.keystroke.events.{Event, InsertChar, RunCommand, UnhandledEvent}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, EditorPane, Focus, PaneId}
import com.serenity.ui.layout.Layout
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A Shift+Alt letter chord reaches the router as the chord (from KEY_PRESSED) and then as the character the OS typed
  * for it (KEY_TYPED). Only a chord that is bound is swallowed with its character; one nothing binds types exactly what
  * the OS delivered, such as the macOS Option+Shift glyphs.
  */
class AltShiftChordTypingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val editorPaneId = PaneId(0)

  private def editorState(osName: String): AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs(osName)),
        layout = Layout(
          editorPanes = Map(editorPaneId -> EditorPane.withBuffer(editorPaneId, BufferId(0))),
          activeEditorPaneId = Some(editorPaneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(editorPaneId))
        ),
        focus = Focus.EditorPane(editorPaneId)
      )
    )

  private def route(osName: String, strokes: KeyStrokeInfo*): List[Event] =
    InputRouter
      .create[IO, Event](FocusedInputTranslator.forState(editorState(osName)))
      .flatMap(router => router.eventStream(Stream.emits(strokes.toList)).compile.toList)
      .unsafeRunSync()

  private def chord(letter: Char) = KeyStrokeInfo(InputKey.Character, Some(letter), Set(Modifier.Alt, Modifier.Shift))
  private def typed(char: Char)   = KeyStrokeInfo(InputKey.Character, Some(char), Set.empty)

  "An unbound Option+Shift+J on macOS" should "type the glyph the OS delivered, unchanged" in {
    val events = route("Mac OS X", chord('j'), typed('Í'))

    events.collect { case InsertChar(char) => char } shouldBe List('Í')
    events.head shouldBe a[UnhandledEvent[?]]
  }

  "A bound Shift+Alt+A" should "run the block-comment command and type nothing" in {
    val events = route("Linux", chord('a'), typed('A'))

    events.head shouldBe RunCommand("toggle-block-comment")
    events.collect { case InsertChar(char) => char } shouldBe Nil
  }

  it should "leave the next ordinary character alone" in {
    val events = route("Linux", chord('a'), typed('A'), typed('x'))

    events.collect { case InsertChar(char) => char } shouldBe List('x')
  }

  "A plain character after an unbound chord" should "still type" in {
    route("Linux", chord('j'), typed('J'), typed('x')).collect { case InsertChar(char) => char } shouldBe
      List('J', 'x')
  }
