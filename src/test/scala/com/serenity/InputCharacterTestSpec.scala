package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Comprehensive character input testing to identify and fix character input bugs. Tests all printable ASCII
  * characters, full alphabet, punctuation, and complete phrases.
  */
class InputCharacterTestSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Character Input Translation and Processing"

  it should "translate all lowercase letters correctly" in new InputFixture:
    val lowercaseLetters = "abcdefghijklmnopqrstuvwxyz"

    for char <- lowercaseLetters do
      val info  = KeyStrokeInfo(InputKey.Character, Some(char), Set.empty)
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  it should "translate all uppercase letters correctly" in new InputFixture:
    val uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    for char <- uppercaseLetters do
      val info  = KeyStrokeInfo(InputKey.Character, Some(char), Set(Modifier.Shift))
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  it should "translate all digits correctly" in new InputFixture:
    val digits = "0123456789"

    for char <- digits do
      val info  = KeyStrokeInfo(InputKey.Character, Some(char), Set.empty)
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  it should "translate all basic punctuation correctly" in new InputFixture:
    val basicPunctuation = ".,;:!?'\""

    for char <- basicPunctuation do
      val info  = KeyStrokeInfo(InputKey.Character, Some(char), Set.empty)
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  it should "translate all special characters correctly" in new InputFixture:
    val specialChars = "()[]{}+-*/=<>@#$%^&_|\\`~"

    for char <- specialChars do
      val info  = KeyStrokeInfo(InputKey.Character, Some(char), Set.empty)
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  it should "translate space character correctly" in new InputFixture:
    val info  = KeyStrokeInfo(InputKey.Character, Some(' '), Set.empty)
    val event = translator.translate(info)
    event.shouldBe(InsertChar(' '))

  behavior of "End-to-End Character Processing"

  it should "process lowercase 'the quick brown fox jumps over the lazy dog' correctly" in new InputFixture:
    val phrase   = "the quick brown fox jumps over the lazy dog"
    val bufferId = setupBuffer("")

    phrase.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process uppercase 'THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG' correctly" in new InputFixture:
    val phrase   = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
    val bufferId = setupBuffer("")

    phrase.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process mixed case phrase with punctuation correctly" in new InputFixture:
    val phrase   = "The Quick Brown Fox Jumps Over The Lazy Dog!"
    val bufferId = setupBuffer("")

    phrase.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process complete sentence with all punctuation marks" in new InputFixture:
    val phrase   = "Hello, World! How are you? I'm fine. (Thanks for asking) - it's 100% true."
    val bufferId = setupBuffer("")

    phrase.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process programming syntax correctly" in new InputFixture:
    val code = """def hello(): String = {
  "Hello, World!"
}""".replace("\r\n", "\n")
    val bufferId = setupBuffer("")

    code.foreach { char =>
      val event = if char == '\n' then NewLine else InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe code

  behavior of "Specific Character Issue Investigation"

  it should "specifically test uppercase 'O' character" in new InputFixture:
    val bufferId = setupBuffer("")
    stateManager.applyEvent(InsertChar('O')).unsafeRunSync()
    getBufferContent(bufferId).shouldBe("O")

  it should "test lowercase 'o' character" in new InputFixture:
    val bufferId = setupBuffer("")
    stateManager.applyEvent(InsertChar('o')).unsafeRunSync()
    getBufferContent(bufferId).shouldBe("o")

  it should "test sequence with both 'O' and 'o' characters" in new InputFixture:
    val phrase   = "Hello World"
    val bufferId = setupBuffer("")
    phrase.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    getBufferContent(bufferId).shouldBe(phrase)

  it should "test all vowels in upper and lower case" in new InputFixture:
    val vowels   = "AEIOUaeiou"
    val bufferId = setupBuffer("")
    vowels.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    getBufferContent(bufferId).shouldBe(vowels)

  behavior of "KeyStrokeInfo Translation Testing"

  it should "properly translate KeyStrokeInfo for all printable ASCII" in new InputFixture:
    for charCode <- 32 to 126 do
      val char = charCode.toChar
      val info = KeyStrokeInfo(InputKey.Character, Some(char), Set.empty)
      info.keyType.shouldBe(InputKey.Character)
      info.character.shouldBe(Some(char))
      info.modifiers.shouldBe(empty)
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  it should "handle modifier keys correctly for uppercase letters" in new InputFixture:
    val uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    for char <- uppercaseLetters do
      val info = KeyStrokeInfo(InputKey.Character, Some(char), Set(Modifier.Shift))
      info.hasShift.shouldBe(true)
      info.character.shouldBe(Some(char))
      val event = translator.translate(info)
      event.shouldBe(InsertChar(char))

  behavior of "Edge Cases and Error Conditions"

  it should "handle rapid sequence of problematic characters" in new InputFixture:
    val problematicSequence = "OoPpQqRrSs!@#$%"
    val bufferId            = setupBuffer("")
    problematicSequence.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    getBufferContent(bufferId).shouldBe(problematicSequence)

  it should "maintain cursor position correctly during character insertion" in new InputFixture:
    val bufferId = setupBuffer("")
    val text     = "Hello"

    text.zipWithIndex.foreach {
      case (char, index) =>
        stateManager.applyEvent(InsertChar(char)).unsafeRunSync()
        val state  = stateManager.getCurrentState.unsafeRunSync()
        val pane   = getCurrentPane(state)
        val buffer = pane.bufferId.flatMap(state.buffers.get).get
        buffer.cursors.head.column.shouldBe(index + 1)
    }

  trait InputFixture:
    val translator: TextEntryTranslator = new TextEntryTranslator()

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    def setupBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      val state    = stateManager.getCurrentState.unsafeRunSync()
      val paneId   = state.layout.editorPanes.keys.head
      stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
      bufferId

    def getBufferContent(bufferId: BufferId): String =
      val state = stateManager.getCurrentState.unsafeRunSync()
      state.buffers.get(bufferId).map(_.content.collect()).getOrElse("")

    def getCurrentPane(state: AppState): EditorPane =
      val paneId = state.layout.editorPanes.keys.head
      state.layout.editorPanes(paneId)
