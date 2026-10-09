package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.SpellingStateFixture.*
import com.serenity.command.{CommandIntent, SpellingIntent}
import com.serenity.keystroke.events.MouseClick
import com.serenity.spellcheck.{SpellChecker, SpellSuggester}
import com.serenity.state.models.*
import com.serenity.state.reducers.SpellingReplacementReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1939: the writer corrects a misspelling from the editor's context menu or from a popup at the cursor. */
class SpellingMenuSpec extends AnyFlatSpec with Matchers:

  private def menuOf(state: AppState): ContextMenu =
    state.contextMenuSurface.map(_.content) match
      case Some(SurfaceContent.ContextMenu(menu)) => menu
      case other                                  => fail(s"Expected an open context menu, got $other")

  private def rightClick(state: AppState, at: CursorPosition): AppState =
    val ref = Ref.of[IO, AppState](state).unsafeRunSync()
    val menus = new EditorContextMenuHitTesting(
      EditorContextMenuHitTestingPort(
        currentState = ref.get,
        applyReducerResult = (result, _) => ref.set(result.state),
        resolveMouseTarget = (_, current) => IO.pure(current.activeBuffer.map(buffer => (paneId, buffer, at))),
        spellingItems = (current, buffer, position) =>
          IO.pure(
            SpellChecker
              .misspellingAt(current, buffer, position)
              .toList
              .flatMap(found => SpellingMenu.items(found, SpellSuggester.suggest(found.word, dictionary)))
          )
      )
    )
    menus.openEditorContextMenu(MouseClick(1, 1), state).unsafeRunSync()
    ref.get.unsafeRunSync()

  "The editor context menu" should "lead with suggestions, then add and ignore, on a misspelled word" in {
    val opened = rightClick(editor("a wrold here"), CursorPosition(0, 4))
    val labels = menuOf(opened).items.map(_.label)

    labels.take(1) shouldBe List("world")
    labels should contain allOf ("Add “wrold” to Dictionary", "Ignore Once", "Ignore All", "Copy")
    labels.indexOf("Ignore All") should be < labels.indexOf("Copy")
  }

  it should "offer nothing about spelling on a correctly spelled word" in {
    val labels = menuOf(rightClick(editor("a wrold here"), CursorPosition(0, 10))).items.map(_.label)

    labels should not contain "Ignore Once"
    labels.headOption shouldBe Some("Copy")
  }

  it should "carry the clicked word, not the cursor's, in each action" in {
    val state   = editor("a wrold here and wrld", CursorPosition(0, 0))
    val opened  = rightClick(state, CursorPosition(0, 18))
    val intents = menuOf(opened).items.map(_.command.intent)

    intents.headOption shouldBe Some(CommandIntent.Spelling(SpellingIntent.Replace(0, 17, 21, "wrld", "world")))
    intents should contain(CommandIntent.Spelling(SpellingIntent.AddToDictionary("wrld")))
    intents should contain(CommandIntent.Spelling(SpellingIntent.IgnoreOnce(0, 17, 21, "wrld")))
    intents should contain(CommandIntent.Spelling(SpellingIntent.IgnoreEverywhere("wrld")))
  }

  "Choosing a suggestion from the menu" should "replace the word it was offered for" in {
    val opened = rightClick(editor("a wrold here"), CursorPosition(0, 4))
    val choice = menuOf(opened).items.headOption.map(_.command.intent)

    val replaced = choice match
      case Some(CommandIntent.Spelling(SpellingIntent.Replace(line, start, end, misspelled, replacement))) =>
        SpellingReplacementReducer.replace(opened.withoutContextMenu, line, start, end, misspelled, replacement)
      case other => fail(s"Expected a replacement, got $other")

    replaced.map(result => textOf(result.state)) shouldBe Some("a world here")
  }

  "SpellingMenu.items" should "offer at most five suggestions before the other actions" in {
    val found = com.serenity.spellcheck.Misspelling(0, 0, 3, "xyz")
    val items = SpellingMenu.items(found, List("a", "b", "c", "d", "e", "f", "g"))

    items.map(_.label) shouldBe List("a", "b", "c", "d", "e", "Add “xyz” to Dictionary", "Ignore Once", "Ignore All")
  }

  it should "still offer the other actions when there is nothing to suggest" in {
    SpellingMenu.items(com.serenity.spellcheck.Misspelling(0, 0, 3, "xyz"), Nil).map(_.label) shouldBe
      List("Add “xyz” to Dictionary", "Ignore Once", "Ignore All")
  }
