package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.input.{InProcessClipboard, InputRouter}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import org.jline.terminal.Size
import org.jline.terminal.impl.DumbTerminal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Reproduction probe for the "TUI drops characters while typing on kitty" report.
  *
  * On a kitty-keyboard-protocol terminal the shell pushes `CSI > 3 u` (disambiguate escape codes | report event types,
  * `TerminalShell.KittyPushFlags`). Under those flags every bare-modifier edge -- the Shift press *and* release that
  * bracket every capital letter or shifted symbol during ordinary prose -- is delivered as its own `CSI 57441 u` /
  * `CSI 57441;1:3 u` sequence, each starting with a lone `ESC` byte that drives `TerminalInputHandler`'s
  * ESC-disambiguation path (`awaitAfterEsc`). Ordinary typing therefore churns that path continuously, interleaved with
  * the plain-text bytes of the letters themselves -- a mix the existing `TerminalInputHandlerSpec` cases never exercise
  * together at volume.
  *
  * These specs feed a large, realistic kitty byte stream through the real handler (via `readerOverride`, no OS pipe)
  * and assert that not one text character is lost or reordered.
  */
class TerminalInputHandlerKittyBurstSpec extends AnyFlatSpec with Matchers:

  private val StreamTimeout = 60.seconds
  private val esc           = 0x1b.toByte

  // These specs feed every byte up front, so an escape sequence's continuation is always already available -- what they
  // verify is that the drain/decode never drops a byte at volume, not the 50ms lone-ESC disambiguation race (covered by
  // TerminalInputHandlerSpec). A generous deadline keeps a starved CI runner from firing that race spuriously and
  // splitting a CSI-u sequence, which would fail these assertions for a reason they aren't about.
  private val EscDeadline = 10.seconds

  private val translator = new TextEntryTranslator(
    AppConfig.default.withHotkeyConfig(AppConfig.default.inputConfig.hotkeyConfig.forTerminalUse)
  )

  private def bytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)
  private def csi(s: String): Array[Byte]   = esc +: bytes(s"[$s")

  private def structuralTerminal(): DumbTerminal =
    val terminal = new DumbTerminal(
      "test",
      "xterm-256color",
      new ByteArrayInputStream(Array.emptyByteArray),
      new ByteArrayOutputStream(),
      StandardCharsets.UTF_8
    )
    terminal.setSize(new Size(80, 24))
    terminal

  /** All decoded keystrokes for a fully-fed-then-EOF input, read off the raw pre-translation stream so the assertion is
    * about what the decoder/handler produced, independent of any translator behaviour.
    */
  private def keystrokesFrom(input: Array[Byte]): List[KeyStrokeInfo] =
    val reader = new FakeTerminalReader
    reader.feed(input)
    reader.feedEof()
    val program = for
      clipboard <- InProcessClipboard[IO]
      router    <- InputRouter.create[IO, Event](translator)
      handler <- TerminalInputHandler.create(
        structuralTerminal(),
        router,
        clipboard,
        escDeadline = EscDeadline,
        readerOverride = Some(reader)
      )
      strokes <- handler.keyStrokeInfoStream.compile.toList
    yield strokes
    program.unsafeRunTimed(StreamTimeout).getOrElse(fail("timed out waiting for keystrokes"))

  // Kitty bare-Shift edges around a capital: Shift press, then the shifted char as plain text, then Shift release.
  private val shiftDown = csi("57441u")
  private val shiftUp   = csi("57441;1:3u")

  // Regression for the drop found investigating this report: when a whole input plus its trailing EOF are already
  // buffered before the read loop drains (any short, at-once input), `drainAvailable` used to `tryTake` the EOF while
  // batching bytes and silently discard it, so `emitEof` never ran and the event stream hung open. `keystrokesFrom`
  // consumes to stream completion, so a hang here surfaces as a timeout.
  "an input fully buffered together with its EOF" should "complete the stream rather than hang" in {
    keystrokesFrom(bytes("ab")).map(_.keyType) shouldBe List(
      InputKey.Character,
      InputKey.Character,
      InputKey.EOF
    )
  }

  "typing a word starting with a capital, kitty-style" should "not drop the letters between the Shift edges" in {
    val word    = shiftDown ++ bytes("H") ++ shiftUp ++ bytes("ello ")
    val strokes = keystrokesFrom(word)
    strokes.filter(_.keyType == InputKey.Character).flatMap(_.character).mkString shouldBe "Hello "
  }

  "a long burst of capitalised prose over kitty" should "deliver every text character in order" in {
    val unit     = shiftDown ++ bytes("H") ++ shiftUp ++ bytes("ello ")
    val reps     = 500
    val input    = Array.fill(reps)(unit).flatten
    val expected = "Hello " * reps
    val strokes  = keystrokesFrom(input)
    strokes.filter(_.keyType == InputKey.Character).flatMap(_.character).mkString shouldBe expected
  }

  "plain text interleaved with complete escape sequences at volume" should "lose neither the text nor the sequences" in {
    // Each 'a' is plain text (no ESC); each ArrowUp is a full ESC-prefixed sequence -- so this alternates the two
    // decode paths, hammering awaitAfterEsc once per pair, the churn kitty prose produces via its modifier edges.
    val reps  = 1000
    val input = Array.fill(reps)(bytes("a") ++ csi("A")).flatten
    val strokes = keystrokesFrom(input)
      .filter(info => info.keyType == InputKey.Character || info.keyType == InputKey.ArrowUp)
    strokes.count(_.keyType == InputKey.Character) shouldBe reps
    strokes.count(_.keyType == InputKey.ArrowUp) shouldBe reps
    strokes shouldBe List
      .fill(reps)(
        List(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty), KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty))
      )
      .flatten
  }
