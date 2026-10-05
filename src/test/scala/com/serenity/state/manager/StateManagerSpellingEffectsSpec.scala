package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.SpellingStateFixture.*
import com.serenity.command.SpellingIntent
import com.serenity.keystroke.events.Event
import com.serenity.spellcheck.SpellSuggester
import com.serenity.state.effects.Lane
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1939: the commands behind the spelling menu, run against a live state. */
class StateManagerSpellingEffectsSpec extends AnyFlatSpec with Matchers:

  final private class Fixture(initial: AppState):
    val state: Ref[IO, AppState]          = Ref.of[IO, AppState](initial).unsafeRunSync()
    val addedWords: Ref[IO, List[String]] = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val effects: Ref[IO, List[AppEffect]] = Ref.of[IO, List[AppEffect]](Nil).unsafeRunSync()
    val searched: Ref[IO, List[String]]   = Ref.of[IO, List[String]](Nil).unsafeRunSync()

    private val editor = new EffectEditorPort:
      def enqueueEvent(event: Event): IO[Unit]                                                  = IO.unit
      def commitState(newState: AppState, fallbackState: AppState): IO[Unit]                    = state.set(newState)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit]                    = IO.unit
      def scheduleDocumentAnalysis(): IO[Unit]                                                  = IO.unit
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit]                              = IO.unit
      def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit]                               = job
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] = IO.unit
      override def spellingSuggestions: String => IO[List[String]] =
        word => searched.update(_ :+ word).as(SpellSuggester.suggest(word, dictionary))

    private val spelling = new StateManagerSpellingEffects(
      editor,
      state.get,
      effect => effects.update(_ :+ effect),
      word => addedWords.update(_ :+ word)
    )

    def run(intent: SpellingIntent): AppState =
      spelling.interpret(intent).unsafeRunSync()
      state.get.unsafeRunSync()

  "ShowSuggestions" should "open a menu of corrections at the misspelling under the cursor" in {
    val fixture = new Fixture(editor("a wrold here", CursorPosition(0, 4)))

    val opened = fixture.run(SpellingIntent.ShowSuggestions)

    opened.contextMenuSurface.map(_.content) match
      case Some(SurfaceContent.ContextMenu(menu)) =>
        val labels = menu.items.map(_.label)
        labels.headOption shouldBe Some("world")
        labels.takeRight(3) shouldBe List("Add “wrold” to Dictionary", "Ignore Once", "Ignore All")
      case other => fail(s"Expected the suggestion menu, got $other")
    fixture.searched.get.unsafeRunSync() shouldBe List("wrold")
  }

  it should "open nothing, and search for nothing, when the cursor is not on a misspelling" in {
    val fixture = new Fixture(editor("a wrold here", CursorPosition(0, 10)))

    fixture.run(SpellingIntent.ShowSuggestions).contextMenuSurface shouldBe None
    fixture.searched.get.unsafeRunSync() shouldBe Nil
  }

  "Replace" should "put the correction in the buffer and record the undo step" in {
    val fixture = new Fixture(editor("a wrold here"))

    val replaced = fixture.run(SpellingIntent.Replace(0, 2, 7, "wrold", "world"))

    textOf(replaced) shouldBe "a world here"
    fixture.effects.get.unsafeRunSync().size shouldBe 1
  }

  it should "leave a buffer alone when the word it was offered for has changed" in {
    val fixture = new Fixture(editor("a wrold here"))

    textOf(fixture.run(SpellingIntent.Replace(0, 2, 7, "wurld", "world"))) shouldBe "a wrold here"
    fixture.effects.get.unsafeRunSync() shouldBe Nil
  }

  "AddToDictionary" should "hand the word to the persisted custom words" in {
    val fixture = new Fixture(editor("a wrold here"))

    fixture.run(SpellingIntent.AddToDictionary("wrold"))

    fixture.addedWords.get.unsafeRunSync() shouldBe List("wrold")
  }

  "IgnoreOnce and IgnoreEverywhere" should "hide the misspelling at the cursor without a trip to the menu" in {
    val once = new Fixture(editor("wrold and wrold", CursorPosition(0, 2)))
    val all  = new Fixture(editor("wrold and wrold", CursorPosition(0, 2)))

    flaggedWords(once.run(SpellingIntent.IgnoreOnceAtCursor)) shouldBe List("wrold")
    flaggedWords(all.run(SpellingIntent.IgnoreEverywhereAtCursor)) shouldBe Nil
  }
