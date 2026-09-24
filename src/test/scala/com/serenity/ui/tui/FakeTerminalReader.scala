package com.serenity.ui.tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, Semaphore, TimeUnit}

import org.jline.terminal.Size
import org.jline.terminal.impl.DumbTerminal
import org.jline.utils.NonBlockingReader

/** A deterministic double for JLine's `NonBlockingReader`, backed by an in-memory queue instead of a real OS pipe.
  *
  * `TerminalInputHandler` and `TerminalShell`'s own startup negotiation both read off a `Terminal`'s `reader()`, and
  * earlier versions of the specs that drive them fed it through a real `DumbTerminal` over a live
  * `PipedInputStream`/`PipedOutputStream` pair. A `DumbTerminal` spins up its own background pump thread eagerly at
  * construction (confirmed against JLine 3.27.1's source), so a still-open real OS pipe plus that thread is exactly the
  * combination that can surface as `java.io.IOException: Pipe broken` under full-suite load (#1358) -- and,
  * independently, the reader fiber itself can be starved past a disambiguation deadline under load (#1314). This queue
  * has no OS pipe and no extra thread: bytes land exactly when a test calls [[feed]].
  *
  * Reads behave like a real, healthy terminal: bytes already queued resolve immediately, and only a genuinely empty
  * queue blocks (or, for a timed read, waits out the deadline).
  */
final class FakeTerminalReader extends NonBlockingReader:
  private val chars              = new LinkedBlockingQueue[Integer]()
  private val EofSentinel: Int   = -1
  private val PauseSentinel: Int = -2
  private val waitingReads       = new Semaphore(0)

  /** `NonBlockingReader.read()` hands out decoded UTF-16 code units, one per call -- JLine wraps the underlying byte
    * stream in a charset decoder, it does not hand out raw bytes (`TerminalInputHandler.toUtf8Bytes` re-encodes them
    * for exactly this reason). `input` is decoded here so a caller can keep building UTF-8 byte arrays -- the natural
    * representation for an escape sequence or pasted text -- without also having to decode them itself.
    */
  def feed(input: Array[Byte]): Unit =
    new String(input, StandardCharsets.UTF_8).foreach(c => chars.put(c.toInt))
  def feedEof(): Unit = chars.put(EofSentinel)

  /** A pause in the input: the read that reaches it reports `READ_EXPIRED`, exactly what the handler's own deadline
    * concludes when nothing follows a lone `ESC`. A spec that marks the pause explicitly can leave that deadline long
    * enough never to fire, so a scheduling stall cannot split an escape sequence that was written in one piece.
    */
  def feedPause(): Unit = chars.put(PauseSentinel)

  /** Blocks until everything fed so far has been read and a read has found nothing left: the reader is idle. */
  @annotation.tailrec
  def awaitWaitingRead(): Unit =
    waitingReads.acquire()
    if !chars.isEmpty then awaitWaitingRead()

  override def read(timeout: Long, isPeek: Boolean): Int =
    if chars.isEmpty then waitingReads.release()
    val next =
      if timeout <= 0 then chars.take().intValue()
      else Option(chars.poll(timeout, TimeUnit.MILLISECONDS)).fold(NonBlockingReader.READ_EXPIRED)(_.intValue())
    if next == EofSentinel then
      chars.put(EofSentinel) // leave EOF latched for any subsequent read
      NonBlockingReader.EOF
    else if next == PauseSentinel then NonBlockingReader.READ_EXPIRED
    else next

  override def readBuffered(b: Array[Char], off: Int, len: Int, timeout: Long): Int =
    throw new UnsupportedOperationException("not used by the handler's read loop")

  override def shutdown(): Unit = ()

object FakeTerminalReader:

  /** A `DumbTerminal` whose `reader()` returns a fresh [[FakeTerminalReader]] instead of one built from a real input
    * stream -- every caller of `terminal.reader()` (`TerminalInputHandler`, `TerminalShell`'s keyboard-protocol
    * negotiation) sees the same fake reader, fed by the returned handle. `output` receives whatever the session under
    * test writes, exactly as a real terminal's output stream would.
    */
  def dumbTerminal(size: Size, output: OutputStream): (DumbTerminal, FakeTerminalReader) =
    val fakeReader = new FakeTerminalReader
    val terminal = new DumbTerminal(
      "test",
      "xterm-256color",
      new ByteArrayInputStream(Array.emptyByteArray),
      output,
      StandardCharsets.UTF_8
    ):
      override def reader(): NonBlockingReader = fakeReader
    terminal.setSize(size)
    (terminal, fakeReader)

  def dumbTerminal(size: Size): (DumbTerminal, FakeTerminalReader) =
    dumbTerminal(size, new ByteArrayOutputStream())
