package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Comprehensive character input testing to identify and fix character input bugs. Tests all printable ASCII
  * characters, full alphabet, punctuation, and complete phrases.
  */
class InputCharacterTestSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Character Input Translation and Processing"

  it should "translate all lowercase letters correctly" in new InputFixture:
    val lowercaseLetters = "abcdefghijklmnopqrstuvwxyz"

    for char <- lowercaseLetters do
      val keyStroke = createKeyStroke(char)
      val event     = translator.translate(keyStroke)

      event.shouldBe(InsertChar(char))

  it should "translate all uppercase letters correctly" in new InputFixture:
    val uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    for char <- uppercaseLetters do
      val keyStroke = createKeyStrokeWithShift(char)
      val event     = translator.translate(keyStroke)

      event.shouldBe(InsertChar(char))

  it should "translate all digits correctly" in new InputFixture:
    val digits = "0123456789"

    for char <- digits do
      val keyStroke = createKeyStroke(char)
      val event     = translator.translate(keyStroke)

      event.shouldBe(InsertChar(char))

  it should "translate all basic punctuation correctly" in new InputFixture:
    val basicPunctuation = ".,;:!?'\""

    for char <- basicPunctuation do
      val keyStroke = createKeyStroke(char)
      val event     = translator.translate(keyStroke)

      event.shouldBe(InsertChar(char))

  it should "translate all special characters correctly" in new InputFixture:
    val specialChars = "()[]{}+-*/=<>@#$%^&_|\\`~"

    for char <- specialChars do
      val keyStroke = createKeyStroke(char)
      val event     = translator.translate(keyStroke)

      event.shouldBe(InsertChar(char))

  it should "translate space character correctly" in new InputFixture:
    val keyStroke = createKeyStroke(' ')
    val event     = translator.translate(keyStroke)

    event.shouldBe(InsertChar(' '))

  behavior of "End-to-End Character Processing"

  it should "process lowercase 'the quick brown fox jumps over the lazy dog' correctly" in new InputFixture:
    val phrase   = "the quick brown fox jumps over the lazy dog"
    val bufferId = setupBuffer("")

    // Process each character through the full pipeline
    phrase.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process uppercase 'THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG' correctly" in new InputFixture:
    val phrase   = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
    val bufferId = setupBuffer("")

    // Process each character through the full pipeline
    phrase.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process mixed case phrase with punctuation correctly" in new InputFixture:
    val phrase   = "The Quick Brown Fox Jumps Over The Lazy Dog!"
    val bufferId = setupBuffer("")

    // Process each character through the full pipeline
    phrase.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process complete sentence with all punctuation marks" in new InputFixture:
    val phrase   = "Hello, World! How are you? I'm fine. (Thanks for asking) - it's 100% true."
    val bufferId = setupBuffer("")

    // Process each character through the full pipeline
    phrase.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "process programming syntax correctly" in new InputFixture:
    val code     = """def hello(): String = {
  "Hello, World!"
}"""
    val bufferId = setupBuffer("")

    // Process each character and newlines
    code.foreach { char =>
      val event = if char == '\n' then NewLine else InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe code

  behavior of "Specific Character Issue Investigation"

  it should "specifically test uppercase 'O' character" in new InputFixture:
    val bufferId = setupBuffer("")

    val event = InsertChar('O')
    stateManager.applyEvent(event).unsafeRunSync()

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe("O")

  it should "test lowercase 'o' character" in new InputFixture:
    val bufferId = setupBuffer("")

    val event = InsertChar('o')
    stateManager.applyEvent(event).unsafeRunSync()

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe("o")

  it should "test sequence with both 'O' and 'o' characters" in new InputFixture:
    val phrase   = "Hello World"
    val bufferId = setupBuffer("")

    phrase.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(phrase)

  it should "test all vowels in upper and lower case" in new InputFixture:
    val vowels   = "AEIOUaeiou"
    val bufferId = setupBuffer("")

    vowels.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(vowels)

  behavior of "KeyStroke Translation Layer Testing"

  it should "properly translate keystrokes from Lanterna for all printable ASCII" in new InputFixture:
    // Test all printable ASCII characters (32-126)
    for charCode <- 32 to 126 do
      val char          = charCode.toChar
      val keyStroke     = createKeyStroke(char)
      val keyStrokeInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)

      // Verify the keystroke info is correct
      keyStrokeInfo.keyType.shouldBe(KeyType.Character)
      keyStrokeInfo.character.shouldBe(Some(char))
      keyStrokeInfo.modifiers.shouldBe(empty)

      // Verify translation to event
      val event = translator.translate(keyStroke)
      event.shouldBe(InsertChar(char))

  it should "handle modifier keys correctly for uppercase letters" in new InputFixture:
    val uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    for char <- uppercaseLetters do
      val keyStroke     = createKeyStrokeWithShift(char)
      val keyStrokeInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)

      // Verify shift modifier is detected
      keyStrokeInfo.hasShift.shouldBe(true)
      keyStrokeInfo.character.shouldBe(Some(char))

      // However, the character converter should still work since it checks for printable chars
      val event = translator.translate(keyStroke)
      event.shouldBe(InsertChar(char))

  behavior of "Edge Cases and Error Conditions"

  it should "handle rapid sequence of problematic characters" in new InputFixture:
    val problematicSequence = "OoPpQqRrSs!@#$%"
    val bufferId            = setupBuffer("")

    problematicSequence.foreach { char =>
      val event = InsertChar(char)
      stateManager.applyEvent(event).unsafeRunSync()
    }

    val finalContent = getBufferContent(bufferId)
    finalContent.shouldBe(problematicSequence)

  it should "maintain cursor position correctly during character insertion" in new InputFixture:
    val bufferId = setupBuffer("")
    val text     = "Hello"

    text.zipWithIndex.foreach {
      case (char, index) =>
        val event = InsertChar(char)
        stateManager.applyEvent(event).unsafeRunSync()

        // Check cursor position after each character
        val state = stateManager.getCurrentState.unsafeRunSync()
        val pane  = getCurrentPane(state)
        pane.cursors.head.column.shouldBe(index + 1)
    }

  trait InputFixture:
    val translator: TextEntryTranslator = new TextEntryTranslator()
    val stateManager: StateManager      = StateManager.apply.unsafeRunSync()

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

    // Helper methods to create KeyStroke objects for testing
    def createKeyStroke(char: Char): KeyStroke =
      new KeyStroke(char, false, false, false)

    def createKeyStrokeWithShift(char: Char): KeyStroke =
      new KeyStroke(char, false, false, true)

    def createKeyStrokeWithCtrl(char: Char): KeyStroke =
      new KeyStroke(char, true, false, false)

    def createKeyStrokeWithAlt(char: Char): KeyStroke =
      new KeyStroke(char, false, true, false)
