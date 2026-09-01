package com.serenity.ui.tui

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.{FiberIO, IO, Ref}
import cats.syntax.all.*
import com.serenity.input.{InputHandler, InputRouter, ModifierTapDetector, ModifierTapState, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import fs2.Stream
import org.jline.terminal.Terminal
import org.jline.utils.NonBlockingReader

import TerminalInputDecoder.DecodedToken

/** [[InputHandler]] for TUI mode: decodes JLine's raw key input stream to the same [[KeyStrokeInfo]] / [[Event]]
  * vocabulary `SwingInputHandler` produces from AWT, so nothing downstream of the handler boundary (translators,
  * keymaps, `InputRouter`, reducers) needs to know it isn't looking at a Swing component.
  *
  * The byte-level decoding itself is [[TerminalInputDecoder]], a pure function -- this class is the effectful shell
  * around it: it owns the JLine read loop, the short ESC-disambiguation deadline, mouse-move/drag dedup (mirroring
  * `SwingInputHandler`'s latest-wins `MovementSlot`), routing bracketed-paste text through the existing paste event
  * path, and EOF-to-graceful-shutdown.
  */
final class TerminalInputHandler private (
    inputRouter: InputRouter[IO, Event],
    queue: Queue[IO, Option[TerminalInputHandler.QueuedInput]],
    readerFiber: FiberIO[Unit],
    disableModes: IO[Unit],
    focusCallback: AtomicReference[Option[Boolean => Unit]]
) extends InputHandler[IO]:

  import TerminalInputHandler.QueuedInput

  /** Satisfies `AppRuntime.run`'s `registerFocusCallback: (Boolean => Unit) => Unit` capability for TUI mode (#1171):
    * the read loop's own decoding of `CSI I`/`CSI O` (terminal focus reporting, `CSI ?1004h`, enabled by
    * `TerminalShell`) invokes whatever is registered here, mirroring how the Swing window's focus listeners drive the
    * same capability in `Main`'s GUI wiring.
    */
  def registerFocusCallback(callback: Boolean => Unit): Unit = focusCallback.set(Some(callback))

  def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] =
    orderedInputStream.collect { case QueuedInput.Key(info) => info }

  def eventStream: Stream[IO, Event] =
    orderedInputStream.flatMap {
      case QueuedInput.Key(info)      => inputRouter.eventStream(Stream.emit(info))
      case QueuedInput.Direct(event)  => Stream.emit(event)
      case QueuedInput.Movement(slot) => Stream.eval(slot.claim).unNone
    }

  private def orderedInputStream: Stream[IO, QueuedInput] = Stream.fromQueueNoneTerminated(queue)

  def shutdown: IO[Unit] =
    readerFiber.cancel >> disableModes.attempt.void >> queue.offer(None)

object TerminalInputHandler:

  /** How long a lone `ESC` byte is held before it is resolved to a bare [[InputKey.Escape]] rather than the start of a
    * multi-byte escape sequence. 50ms is comfortably above the delay a real terminal introduces between the bytes of
    * one escape sequence (they arrive as a single burst from the pty), and comfortably below the gap a human perceives
    * as sluggish.
    */
  val EscDisambiguationDeadline: FiniteDuration = 50.millis

  private[tui] enum MovementKind:
    case Move, Drag

  sealed private trait MovementState
  final private case class AvailableMovement(event: MouseInputEvent) extends MovementState
  private case object ClaimedMovement                                extends MovementState

  /** Mirrors `SwingInputHandler`'s `MovementSlot`: only the latest move/drag of a burst is ever delivered, so a slow
    * consumer doesn't fall behind replaying stale mouse positions. The producer here is a single sequential read loop
    * (unlike Swing's concurrent AWT listeners), so a plain [[Ref]] update is enough -- no CAS retry loop is needed to
    * guard against concurrent producers.
    */
  final private[tui] class MovementSlot(val kind: MovementKind, initial: MouseInputEvent):
    private val state = Ref.unsafe[IO, MovementState](AvailableMovement(initial))

    def replace(event: MouseInputEvent): IO[Boolean] =
      state.modify {
        case AvailableMovement(_) => (AvailableMovement(event), true)
        case ClaimedMovement      => (ClaimedMovement, false)
      }

    def claim: IO[Option[MouseInputEvent]] =
      state.getAndSet(ClaimedMovement).map {
        case AvailableMovement(event) => Some(event)
        case ClaimedMovement          => None
      }

  sealed private[tui] trait QueuedInput

  private[tui] object QueuedInput:
    final case class Key(info: KeyStrokeInfo)     extends QueuedInput
    final case class Direct(event: Event)         extends QueuedInput
    final case class Movement(slot: MovementSlot) extends QueuedInput

  /** Enable SGR mouse reporting (clicks/drags via 1002, hover-tracking any-motion via 1003, extended coordinates via
    * 1006) and bracketed paste (2004). Sent once at handler creation; [[shutdown]] sends the matching disable sequences
    * so a quitting Serenity doesn't leave the user's regular shell reporting mouse events at it.
    */
  private def enableModes(terminal: Terminal): IO[Unit] =
    IO.blocking {
      val w = terminal.writer()
      w.write("\u001b[?1002h\u001b[?1003h\u001b[?1006h\u001b[?2004h")
      w.flush()
    }

  private def disableModes(terminal: Terminal): IO[Unit] =
    IO.blocking {
      val w = terminal.writer()
      w.write("\u001b[?2004l\u001b[?1006l\u001b[?1003l\u001b[?1002l")
      w.flush()
    }

  /** @param seedBytes
    *   bytes to treat as already-read input, decoded before anything the JLine reader delivers. [[TerminalShell]]'s
    *   startup keyboard-protocol negotiation (#1109) reads directly off the same underlying reader ahead of this
    *   handler's own loop; any bytes it read that turned out not to be part of the negotiation response -- a real
    *   keystroke that raced the negotiation, most likely -- are handed back here rather than silently dropped, so
    *   they're decoded exactly as if this loop had read them itself.
    */
  def create(
    terminal: Terminal,
    inputRouter: InputRouter[IO, Event],
    systemClipboard: SystemClipboard[IO],
    seedBytes: Array[Byte] = Array.emptyByteArray
  ): IO[TerminalInputHandler] =
    for
      queue            <- Queue.unbounded[IO, Option[QueuedInput]]
      rawQueue         <- Queue.unbounded[IO, ReadOutcome]
      latestMovement   <- Ref.of[IO, Option[MovementSlot]](None)
      remainder        <- Ref.of[IO, Array[Byte]](Array.emptyByteArray)
      modifierTapState <- Ref.of[IO, ModifierTapState](ModifierTapState.empty)
      focusCallback    <- IO(new AtomicReference[Option[Boolean => Unit]](None))
      _                <- enableModes(terminal)
      reader            = terminal.reader()
      // rawReadLoop runs on a dedicated fiber so the CE3 compute pool is never blocked waiting for the terminal;
      // guarantee(rawFiber.cancel) tears it down whenever readLoop exits (naturally or via cancellation).
      fiber <- rawReadLoop(reader, rawQueue).start.flatMap { rawFiber =>
        readLoop(rawQueue, queue, latestMovement, remainder, modifierTapState, systemClipboard, seedBytes, focusCallback)
          .guarantee(rawFiber.cancel)
      }.start
    yield new TerminalInputHandler(inputRouter, queue, fiber, disableModes(terminal), focusCallback)

  private enum ReadOutcome:
    case Bytes(value: Array[Byte])
    case Eof
    case Expired

  private def toOutcome(read: Int): ReadOutcome =
    if read == NonBlockingReader.EOF then ReadOutcome.Eof
    else if read == NonBlockingReader.READ_EXPIRED then ReadOutcome.Expired
    else ReadOutcome.Bytes(toUtf8Bytes(read))

  /** JLine's reader hands us decoded Unicode chars, one UTF-16 code unit per `read()`; the pure decoder works on UTF-8
    * bytes (so it stays testable with plain byte arrays, and so the same decoder could serve a raw byte stream too). A
    * char above ASCII is re-encoded to its full UTF-8 byte sequence here before joining the byte buffer, so a
    * multi-byte character never gets split across separate reads on our side.
    *
    * Also used by [[TerminalShell]]'s startup keyboard-protocol negotiation to re-encode any stray bytes it reads off
    * the same reader ahead of this handler's own loop (see `create`'s `seedBytes`).
    */
  private[tui] def toUtf8Bytes(ch: Int): Array[Byte] =
    if ch < 0x80 then Array(ch.toByte) else String.valueOf(ch.toChar).getBytes(StandardCharsets.UTF_8)

  /** Runs on a dedicated fiber; reads raw chars from the JLine reader as fast as they arrive and stuffs each one into
    * [[rawQueue]], so the CE3 compute pool is never blocked waiting for terminal I/O and the pty kernel buffer never
    * fills during a slow dispatch cycle. [[readLoop]] consumes from [[rawQueue]] and can safely use [[Queue.tryTake]]
    * for non-blocking drains without any risk of blocking the compute thread.
    */
  private def rawReadLoop(reader: NonBlockingReader, rawQueue: Queue[IO, ReadOutcome]): IO[Unit] =
    IO.interruptible(reader.read()).map(toOutcome).flatMap {
      case ReadOutcome.Eof => rawQueue.offer(ReadOutcome.Eof)
      case outcome         => rawQueue.offer(outcome) >> rawReadLoop(reader, rawQueue)
    }

  private def readLoop(
    rawQueue: Queue[IO, ReadOutcome],
    queue: Queue[IO, Option[QueuedInput]],
    latestMovement: Ref[IO, Option[MovementSlot]],
    remainder: Ref[IO, Array[Byte]],
    modifierTapState: Ref[IO, ModifierTapState],
    systemClipboard: SystemClipboard[IO],
    seedBytes: Array[Byte],
    focusCallback: AtomicReference[Option[Boolean => Unit]]
  ): IO[Unit] =

    def processTokens(tokens: List[DecodedToken]): IO[Unit] =
      tokens.traverse_(processToken)

    def processToken(token: DecodedToken): IO[Unit] = token match
      case DecodedToken.Key(info) =>
        modifierTapState.update(ModifierTapDetector.otherKeyPressed) >>
          latestMovement.set(None) >> queue.offer(Some(QueuedInput.Key(info)))
      case DecodedToken.Mouse(event) => enqueueMouse(event)
      case DecodedToken.Pasted(text) =>
        // The "paste event path": write the pasted text where a Ctrl+V paste would have left it, then emit the
        // same `Paste` event a hotkey would -- so multi-line pastes are inserted as one paste, not replayed as
        // individual keystrokes (which would fire hotkeys on embedded control characters).
        systemClipboard.writeText(text) >> latestMovement.set(None) >> queue.offer(Some(QueuedInput.Direct(Paste)))
      case DecodedToken.ModifierEdge(modifier, pressed) => processModifierEdge(modifier, pressed)
      case DecodedToken.FocusChanged(focused)           => IO(focusCallback.get().foreach(_.apply(focused)))

    // Drives ModifierTapDetector exactly as SwingInputHandler drives it over AWT modifier press/release events, so
    // `ctrl+ctrl`-style bindings behave identically in both input modes -- only reachable when the terminal answered
    // the kitty protocol's capability query (see TerminalInputDecoder.DecodedToken.ModifierEdge).
    def processModifierEdge(modifier: Modifier, pressed: Boolean): IO[Unit] =
      val now = IO.realTime.map(_.toMillis)
      if pressed then
        now.flatMap { atMillis =>
          modifierTapState.get.flatMap { state =>
            ModifierTapDetector.modifierPressed(state, modifier, atMillis) match
              case ModifierTapDetector.Outcome.Emit(next) =>
                modifierTapState.set(next) >> latestMovement.set(None) >>
                  queue.offer(Some(QueuedInput.Key(KeyStrokeInfo(bareModifierKey(modifier), None, Set.empty))))
              case ModifierTapDetector.Outcome.Pending(next) => modifierTapState.set(next)
          }
        }
      else now.flatMap(atMillis => modifierTapState.update(ModifierTapDetector.modifierReleased(_, modifier, atMillis)))

    def bareModifierKey(modifier: Modifier): InputKey = modifier match
      case Modifier.Ctrl  => InputKey.Ctrl
      case Modifier.Alt   => InputKey.Alt
      case Modifier.Shift => InputKey.Shift
      case Modifier.Meta  => InputKey.Meta

    def enqueueMouse(event: MouseInputEvent): IO[Unit] = event match
      case m: MouseMove => enqueueMovement(MovementKind.Move, m)
      case d: MouseDrag => enqueueMovement(MovementKind.Drag, d)
      case other        => latestMovement.set(None) >> queue.offer(Some(QueuedInput.Direct(other)))

    def enqueueMovement(kind: MovementKind, event: MouseInputEvent): IO[Unit] =
      latestMovement.get.flatMap {
        case Some(slot) if slot.kind == kind =>
          slot.replace(event).flatMap(replaced => if replaced then IO.unit else freshMovementSlot(kind, event))
        case _ => freshMovementSlot(kind, event)
      }

    def freshMovementSlot(kind: MovementKind, event: MouseInputEvent): IO[Unit] =
      val slot = new MovementSlot(kind, event)
      latestMovement.set(Some(slot)) >> queue.offer(Some(QueuedInput.Movement(slot)))

    def emitEof: IO[Unit] =
      queue.offer(Some(QueuedInput.Key(KeyStrokeInfo(InputKey.EOF, None, Set.empty)))) >> queue.offer(None)

    def appendAndDecode(buffer: Array[Byte]): IO[Unit] =
      val result = TerminalInputDecoder.decode(buffer)
      processTokens(result.tokens) >> remainder.set(result.remainder)

    // Drain any characters rawReadLoop has already buffered into rawQueue without blocking. After each blocking take,
    // the dedicated reader fiber (rawReadLoop) has likely raced ahead and queued the next burst of characters; pulling
    // them all here before yielding to the CE3 scheduler batches an entire burst into a single decode call, so fast
    // typing never leaves characters stranded in the queue for a full scheduler quantum.
    def drainAvailable(acc: Array[Byte]): IO[Array[Byte]] =
      rawQueue.tryTake.flatMap {
        case Some(ReadOutcome.Bytes(bs)) => drainAvailable(acc ++ bs)
        case _                            => IO.pure(acc)
      }

    def loop: IO[Unit] =
      remainder.get.flatMap { pending =>
        if pending.length == 1 && pending(0) == 0x1b.toByte then
          IO.race(IO.sleep(EscDisambiguationDeadline), rawQueue.take).flatMap {
            case Left(_) =>
              processTokens(TerminalInputDecoder.decodeFinal(pending)) >> remainder.set(Array.emptyByteArray) >> loop
            case Right(ReadOutcome.Eof) =>
              processTokens(TerminalInputDecoder.decodeFinal(pending)) >> emitEof
            case Right(ReadOutcome.Bytes(bs)) =>
              drainAvailable(pending ++ bs).flatMap(appendAndDecode) >> loop
            case Right(ReadOutcome.Expired) =>
              loop // unreachable: rawReadLoop never enqueues Expired
          }
        else
          rawQueue.take.flatMap {
            case ReadOutcome.Eof =>
              processTokens(TerminalInputDecoder.decodeFinal(pending)) >> emitEof
            case ReadOutcome.Bytes(bs) =>
              drainAvailable(pending ++ bs).flatMap(appendAndDecode) >> loop
            case ReadOutcome.Expired =>
              loop // unreachable: rawReadLoop never enqueues Expired
          }
      }

    // Decode any bytes TerminalShell's startup negotiation read off this same reader but couldn't attribute to its
    // own response (see TerminalInputHandler.create's `seedBytes` doc) before starting the ordinary read loop, so
    // they're never silently lost.
    appendAndDecode(seedBytes) >> loop
