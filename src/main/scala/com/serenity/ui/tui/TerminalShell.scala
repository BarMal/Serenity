package com.serenity.ui.tui

import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicReference

import cats.effect.std.Dispatcher
import cats.effect.{Deferred, IO, Resource}
import com.serenity.ui.layout.ViewportSize
import org.jline.terminal.Terminal.Signal
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.utils.InfoCmp.Capability
import org.jline.utils.NonBlockingReader

import TerminalShell.KeyboardProtocolTier

/** Owns a real JLine terminal for the lifetime of TUI mode: raw input, the alternate screen buffer, a hidden hardware
  * cursor, and `SIGWINCH`/`SIGINT` signal handling. Acquired and released as a [[Resource]] so the terminal is always
  * restored to its original state -- on clean shutdown, on an escaping error, and on fiber cancellation alike, since
  * `Resource`'s release runs on every one of those outcomes.
  *
  * @param pendingInputPrefix
  *   bytes [[TerminalShell.negotiateKeyboardProtocol]] read off `terminal.reader()` while probing for a kitty response
  *   that turned out not to be part of one -- most likely a real keystroke that raced the negotiation window. Empty
  *   whenever the probe cleanly matched a response or cleanly timed out with nothing typed yet. The caller
  *   (`TuiRuntime`) must feed this to [[TerminalInputHandler.create]]'s `seedBytes` so it is decoded rather than lost
  *   -- this class only reads the reader, it never itself decodes or replays these bytes.
  */
final class TerminalShell private (
    private[tui] val terminal: Terminal,
    private val originalAttributes: org.jline.terminal.Attributes,
    private val quitDeferred: Deferred[IO, Unit],
    private val dispatcher: Dispatcher[IO],
    val keyboardProtocolTier: KeyboardProtocolTier,
    val pendingInputPrefix: Array[Byte]
):

  private val pendingResize  = new AtomicReference[Option[ViewportSize]](None)
  private val resizeCallback = new AtomicReference[Option[() => Unit]](None)

  /** The writer [[com.serenity.ui.tui.TerminalRenderSurface]] flushes damage-diffed ANSI bytes through. */
  def writer: PrintWriter = terminal.writer()

  def viewportSize: IO[ViewportSize] = IO(currentViewportSize())

  private def currentViewportSize(): ViewportSize =
    val size = terminal.getSize
    ViewportSize(size.getColumns, size.getRows)

  /** Drains and reports the most recent size a `SIGWINCH` handler observed since the last call, satisfying
    * `AppRuntime.run`'s `checkResize: IO[Option[ViewportSize]]` capability -- `None` when nothing has changed.
    */
  val checkResize: IO[Option[ViewportSize]] = IO(pendingResize.getAndSet(None))

  /** Satisfies `AppRuntime.run`'s `registerResizeCallback: (() => Unit) => Unit` capability: the runtime's own resize
    * bridge is invoked, in addition to updating [[checkResize]]'s pending value, whenever `SIGWINCH` fires.
    */
  def registerResizeCallback(callback: () => Unit): Unit = resizeCallback.set(Some(callback))

  /** Completes once `SIGINT` (Ctrl+C at the raw terminal level, since raw mode disables the kernel's own INT-to-signal
    * delivery via canonical mode) is observed, satisfying `AppRuntime.run`'s `awaitExternalQuit: IO[Unit]` capability
    * so a Ctrl+C still drives a graceful shutdown -- and, via the enclosing [[Resource]], guaranteed terminal
    * restoration -- rather than leaving the terminal corrupted.
    *
    * Backed by a [[Deferred]] rather than a bare `IO.async_` registration: `AppRuntime.run`'s `coordinateExternalQuit`
    * races this against `stateManager.awaitQuit` and cancels whichever side loses -- the ordinary case for a
    * keyboard-driven quit (Ctrl+Q, EOF), where `awaitQuit` wins and this side is the one cancelled. A `Deferred`'s
    * waiter is removed from its join list on cancellation and returns immediately; `IO.async_`'s callback registration
    * has no such fast path and left that race permanently unresolved (discovered integrating the TUI capability bundle
    * for issue #1112 -- this fixes a real hang on every keyboard-driven quit, not just a test artifact).
    */
  val awaitExternalQuit: IO[Unit] = quitDeferred.get

  private[tui] def handleWinch(): Unit =
    val size = currentViewportSize()
    pendingResize.set(Some(size))
    resizeCallback.get().foreach(_.apply())

  /** Invoked synchronously on the JVM's signal-dispatch thread, outside any `IO` context -- the `Dispatcher` acquired
    * alongside this shell is how a plain signal callback completes an `IO`-native `Deferred`.
    */
  private[tui] def handleInt(): Unit =
    dispatcher.unsafeRunAndForget(quitDeferred.complete(()).void)

  private[tui] def restore(): Unit =
    terminal.setAttributes(originalAttributes)
    TerminalShell.disableKeyboardProtocol(terminal, keyboardProtocolTier)
    val _ = terminal.puts(Capability.cursor_normal)
    val _ = terminal.puts(Capability.exit_ca_mode)
    terminal.flush()

object TerminalShell:

  /** Which tier of the CSI-u negotiation ladder (#1109) [[acquire]] settled on, from a startup probe of the terminal:
    *
    *   - [[Kitty]]: the terminal answered the kitty keyboard protocol's `CSI ? u` capability query, so the
    *     "disambiguate escape codes" and "report event types" enhancement flags were pushed. Full parity, including
    *     bare-modifier press/release for double-tap bindings.
    *   - [[ModifyOtherKeys]]: no kitty response arrived, so xterm's `modifyOtherKeys` mode 2 with `formatOtherKeys=1`
    *     was enabled instead. Combo keys (Ctrl+Shift+letter, Ctrl+Enter) decode, but no bare-modifier events exist in
    *     this protocol -- double-tap bindings stay inert (issue #1194 surfaces a recording-time warning for this, see
    *     `CommandRunnerReducer.bareModifierFidelityWarning`).
    *
    * Issue #1194 verified the `[>4;2m` / `[>4;1f` enable sequences (and their `[>4;0m` / `[>4;0f` disable counterparts)
    * below against xterm's own source tree -- `ctlseqs.txt` from github.com/ThomasDickey/xterm-snapshots (the upstream
    * repo the `invisible-island.net/xterm/ctlseqs` reference page is generated from; that page itself was still
    * unreachable through this environment's egress proxy at verification time, same as when this was first implemented
    * for #1109). It confirms: `Pp = 4` selects `modifyOtherKeys` for `XTMODKEYS` (`CSI > Pp ; Pv m`) and
    * `formatOtherKeys` for `XTFMTKEYS` (`CSI > Pp ; Pv f`), and mode 2 plus `formatOtherKeys=1` is exactly the
    * combination that shapes modified-key sequences as CSI-u (`CSI keycode ; modifiers u`) rather than xterm's older
    * `CSI 27 ; modifier ; keycode ~` encoding -- no discrepancy found, no code change needed here.
    *
    * That same source also turned up something #1109 didn't know to look for: xterm does define a query/response for
    * both resources -- `XTQMODKEYS` (`CSI ? Pp m`) and `XTQFMTKEYS` (`CSI ? Pp g`), each answered with an `XTMODKEYS`/
    * `XTFMTKEYS`-shaped reply (`CSI > Pp ; Pv m` / `CSI > Pp ; Pv f`) carrying the resource's current value. That is a
    * real query/response mechanism the way kitty's `CSI ? u` is, so [[negotiateKeyboardProtocol]] uses it: after
    * pushing the `modifyOtherKeys`/`formatOtherKeys` enable sequences, it sends `XTQMODKEYS`/`XTQFMTKEYS` and waits
    * (bounded by [[NegotiationDeadlineMillis]]) for xterm to echo back `Pv = 2` and `Pv = 1` respectively. Only then is
    * [[ModifyOtherKeys]] reported -- a terminal that never replies, or replies with different values, is
    * indistinguishable from one that ignored the negotiation outright, and settles on [[Legacy]] instead, with the
    * enable sequences explicitly reverted so no half-applied state lingers past negotiation.
    */
  enum KeyboardProtocolTier:
    case Kitty, ModifyOtherKeys

    /** Neither negotiation confirmed: the kitty `CSI ? u` query drew no matching response, and the
      * `modifyOtherKeys`/`formatOtherKeys` enable sequences were not confirmed by `XTQMODKEYS`/`XTQFMTKEYS` either (no
      * reply, or a reply reporting different values) -- so they were reverted rather than left assumed-active. Today's
      * uncontested fallback: CSI-u sequences are never expected to arrive, and `TerminalInputDecoder` decodes
      * everything as legacy bytes.
      */
    case Legacy

  private val KittyQuery: String             = "[?u"
  private val KittyPushFlags: String         = "[>3u" // 1 (disambiguate) | 2 (report event types)
  private val KittyPop: String               = "[<u"
  private val ModifyOtherKeysEnable: String  = "[>4;2m"
  private val ModifyOtherKeysDisable: String = "[>4;0m"
  private val FormatOtherKeysEnable: String  = "[>4;1f"
  private val FormatOtherKeysDisable: String = "[>4;0f"
  private val XtqModKeysQuery: String        = "[?4m" // XTQMODKEYS: query the current modifyOtherKeys value.
  private val XtqFmtKeysQuery: String        = "[?4g" // XTQFMTKEYS: query the current formatOtherKeys value.

  /** The `Pp` resource id both `XTMODKEYS`/`XTQMODKEYS` and `XTFMTKEYS`/`XTQFMTKEYS` address for
    * `modifyOtherKeys`/`formatOtherKeys` (xterm's `ctlseqs.txt`, resource table shared by both controls).
    */
  private val ModifyOtherKeysResource: Int = 4

  /** The `Pv` values a confirming `XTMODKEYS`/`XTFMTKEYS` reply must carry for [[negotiateKeyboardProtocol]] to trust
    * that xterm actually applied what [[ModifyOtherKeysEnable]]/[[FormatOtherKeysEnable]] requested.
    */
  private val ConfirmedModifyOtherKeysValue: Int = 2
  private val ConfirmedFormatOtherKeysValue: Int = 1

  /** How long [[acquire]] waits for a kitty `CSI ? flags u` response before falling back to `modifyOtherKeys`, and
    * separately how long it waits for the `XTQMODKEYS`/`XTQFMTKEYS` confirmation replies before falling further back to
    * [[KeyboardProtocolTier.Legacy]]. Comfortably above a local pty round-trip, comfortably below a perceptible startup
    * stall -- applied per phase, so a terminal that answers neither negotiation adds at most two of these to startup.
    */
  private val NegotiationDeadlineMillis: Long = 100L

  /** Acquire a real system terminal in raw mode with the alternate screen active and the hardware cursor hidden;
    * unconditionally restore it -- attributes, screen buffer, cursor visibility, and any pushed keyboard-protocol
    * enhancement -- on release.
    */
  def resource: Resource[IO, TerminalShell] =
    Resource
      .make(IO.blocking(TerminalBuilder.builder().system(true).nativeSignals(true).build()))(terminal =>
        IO.blocking(terminal.close()).attempt.void
      )
      .flatMap(forTerminal)

  /** Build a shell over an already-constructed [[Terminal]] -- the real system terminal in production, or a
    * streams-backed test terminal in specs -- entering raw mode / alternate screen / hidden cursor / negotiated
    * keyboard protocol on acquire and restoring them unconditionally on release. Does not close `terminal`; the caller
    * owns that (see [[resource]]).
    */
  private[tui] def forTerminal(terminal: Terminal): Resource[IO, TerminalShell] =
    Dispatcher.parallel[IO].flatMap { dispatcher =>
      Resource.make(acquire(terminal, dispatcher))(shell => IO.blocking(shell.restore()).attempt.void)
    }

  private def acquire(terminal: Terminal, dispatcher: Dispatcher[IO]): IO[TerminalShell] =
    for
      quitDeferred <- Deferred[IO, Unit]
      shell <- IO.blocking {
        val originalAttributes = terminal.enterRawMode()
        val _                  = terminal.puts(Capability.enter_ca_mode)
        val _                  = terminal.puts(Capability.cursor_invisible)
        terminal.flush()
        val (tier, prefix) = negotiateKeyboardProtocol(terminal)
        val shell          = new TerminalShell(terminal, originalAttributes, quitDeferred, dispatcher, tier, prefix)
        val _              = terminal.handle(Signal.WINCH, _ => shell.handleWinch())
        val _              = terminal.handle(Signal.INT, _ => shell.handleInt())
        shell
      }
    yield shell

  /** Runs the tiered negotiation ladder the #1109 issue calls for: query kitty support first (bounded by
    * [[NegotiationDeadlineMillis]] so an unsupporting terminal, which sends no response at all, can't stall startup);
    * push kitty's enhancement flags if it answered. Otherwise, push `modifyOtherKeys`/`formatOtherKeys` and confirm
    * xterm actually applied them via `XTQMODKEYS`/`XTQFMTKEYS` (again bounded by [[NegotiationDeadlineMillis]]) before
    * reporting [[KeyboardProtocolTier.ModifyOtherKeys]] -- falling back further to [[KeyboardProtocolTier.Legacy]], and
    * reverting the enable sequences, when that confirmation doesn't arrive (#1194's follow-up: this tier was previously
    * assumed from the enable write succeeding, never confirmed from the terminal's own reply).
    *
    * Reads every query response directly off `terminal.reader()` -- the same `NonBlockingReader`
    * [[TerminalInputHandler]] later starts its own read loop over -- which is safe only because this runs to completion
    * before that loop starts (raw mode is entered, negotiation happens, and only then does the caller construct the
    * input handler).
    *
    * Known limitation (tmux pass-through): tmux's `extended-keys` setting controls whether it forwards a kitty (or
    * `XTQMODKEYS`/`XTQFMTKEYS`) response to the client application at all; this negotiation cannot distinguish "tmux
    * ate the query" from "the terminal underneath doesn't support it", so a `Legacy` result under tmux may undersell
    * what the terminal underneath actually supports. Not verified against a real tmux session here -- flagged rather
    * than guessed at.
    *
    * @return
    *   the negotiated tier, plus any bytes read off the reader while probing that were not part of a matched response
    *   (see [[TerminalShell]]'s `pendingInputPrefix` doc) -- accumulated across both negotiation phases when both run,
    *   and always empty for a phase whose probe matched, since a matched response's bytes are legitimately consumed
    *   protocol traffic, not stray input.
    */
  private def negotiateKeyboardProtocol(terminal: Terminal): (KeyboardProtocolTier, Array[Byte]) =
    val writer = terminal.writer()
    writer.write(KittyQuery)
    writer.flush()
    val (kittyMatched, kittyStray) = awaitKittyResponse(terminal.reader(), NegotiationDeadlineMillis)
    if kittyMatched then
      writer.write(KittyPushFlags)
      writer.flush()
      (KeyboardProtocolTier.Kitty, kittyStray)
    else
      writer.write(ModifyOtherKeysEnable)
      writer.write(FormatOtherKeysEnable)
      writer.write(XtqModKeysQuery)
      writer.write(XtqFmtKeysQuery)
      writer.flush()
      val (confirmed, confirmStray) =
        awaitModifyOtherKeysConfirmation(terminal.reader(), NegotiationDeadlineMillis)
      val strayBytes = kittyStray ++ confirmStray
      if confirmed then (KeyboardProtocolTier.ModifyOtherKeys, strayBytes)
      else
        writer.write(ModifyOtherKeysDisable)
        writer.write(FormatOtherKeysDisable)
        writer.flush()
        (KeyboardProtocolTier.Legacy, strayBytes)

  /** A minimal state machine over the expected `ESC [ ? digits u` response, bounded by an overall deadline so a
    * terminal that never responds can't hang startup. Every byte read is accumulated (re-encoded to UTF-8 via
    * [[TerminalInputHandler.toUtf8Bytes]], matching how that class's own read loop treats JLine's decoded chars) so a
    * byte that doesn't fit the expected shape -- most plausibly a real keystroke racing the negotiation window, since
    * this codebase's own escape sequences are the only thing expected on this reader otherwise -- can be handed back to
    * the caller instead of silently dropped; only reset the match state, never the accumulated bytes.
    */
  private def awaitKittyResponse(reader: NonBlockingReader, deadlineMillis: Long): (Boolean, Array[Byte]) =
    val consumed = Array.newBuilder[Byte]

    @annotation.tailrec
    def loop(state: Int, remaining: Long): Boolean =
      if remaining <= 0 then false
      else
        val start = System.currentTimeMillis()
        val ch    = reader.read(remaining)
        val spent = math.max(1L, System.currentTimeMillis() - start)
        if ch == NonBlockingReader.EOF || ch == NonBlockingReader.READ_EXPIRED then false
        else
          consumed ++= TerminalInputHandler.toUtf8Bytes(ch)
          val nextState = state match
            case 0                                       => if ch == 0x1b then 1 else 0
            case 1                                       => if ch == '['.toInt then 2 else 0
            case 2                                       => if ch == '?'.toInt then 3 else 0
            case 3 if ch >= '0'.toInt && ch <= '9'.toInt => 3
            case 3 if ch == 'u'.toInt                    => 4
            case _                                       => 0
          if nextState == 4 then true else loop(nextState, remaining - spent)

    val matched = loop(0, deadlineMillis)
    (matched, if matched then Array.emptyByteArray else consumed.result())

  /** A minimal state machine over the expected `XTMODKEYS`/`XTFMTKEYS` confirmation replies -- `ESC [ > 4 ; 2 m` and
    * `ESC [ > 4 ; 1 f`, in either order, possibly interleaved -- bounded by a single shared deadline so a terminal that
    * answers one, both, or neither can't hang startup either way. As with [[awaitKittyResponse]], every byte read is
    * accumulated so a byte that doesn't fit either expected shape -- most plausibly a real keystroke racing the
    * negotiation window -- can be handed back to the caller instead of silently dropped; a byte that breaks the current
    * partial match only resets the match state, never the accumulated bytes.
    *
    * @return
    *   whether *both* the modifyOtherKeys and formatOtherKeys replies were seen and reported exactly the requested
    *   values ([[ConfirmedModifyOtherKeysValue]] / [[ConfirmedFormatOtherKeysValue]]) -- a reply reporting some other
    *   value means xterm did not end up in the state requested, which is exactly what this confirmation exists to
    *   catch, so that also counts as unconfirmed.
    */
  private def awaitModifyOtherKeysConfirmation(
    reader: NonBlockingReader,
    deadlineMillis: Long
  ): (Boolean, Array[Byte]) =
    val consumed = Array.newBuilder[Byte]

    @annotation.tailrec
    def loop(
      state: Int,
      resourceDigits: String,
      valueDigits: String,
      sawModKeys: Boolean,
      sawFmtKeys: Boolean,
      remaining: Long
    ): (Boolean, Boolean) =
      if (sawModKeys && sawFmtKeys) || remaining <= 0 then (sawModKeys, sawFmtKeys)
      else
        val start = System.currentTimeMillis()
        val ch    = reader.read(remaining)
        val spent = math.max(1L, System.currentTimeMillis() - start)
        if ch == NonBlockingReader.EOF || ch == NonBlockingReader.READ_EXPIRED then (sawModKeys, sawFmtKeys)
        else
          consumed ++= TerminalInputHandler.toUtf8Bytes(ch)
          val isDigit = ch >= '0'.toInt && ch <= '9'.toInt
          state match
            case 0 => loop(if ch == 0x1b then 1 else 0, "", "", sawModKeys, sawFmtKeys, remaining - spent)
            case 1 => loop(if ch == '['.toInt then 2 else 0, "", "", sawModKeys, sawFmtKeys, remaining - spent)
            case 2 => loop(if ch == '>'.toInt then 3 else 0, "", "", sawModKeys, sawFmtKeys, remaining - spent)
            case 3 if isDigit =>
              loop(3, resourceDigits + ch.toChar, valueDigits, sawModKeys, sawFmtKeys, remaining - spent)
            case 3 if ch == ';'.toInt => loop(4, resourceDigits, "", sawModKeys, sawFmtKeys, remaining - spent)
            case 4 if isDigit =>
              loop(4, resourceDigits, valueDigits + ch.toChar, sawModKeys, sawFmtKeys, remaining - spent)
            case 4 if ch == 'm'.toInt || ch == 'f'.toInt =>
              val resource = resourceDigits.toIntOption
              val value    = valueDigits.toIntOption
              val isModKeysUnit = ch == 'm'.toInt && resource.contains(ModifyOtherKeysResource) && value.contains(
                ConfirmedModifyOtherKeysValue
              )
              val isFmtKeysUnit = ch == 'f'.toInt && resource.contains(ModifyOtherKeysResource) && value.contains(
                ConfirmedFormatOtherKeysValue
              )
              loop(0, "", "", sawModKeys || isModKeysUnit, sawFmtKeys || isFmtKeysUnit, remaining - spent)
            case _ => loop(0, "", "", sawModKeys, sawFmtKeys, remaining - spent)

    val (sawModKeys, sawFmtKeys) = loop(0, "", "", sawModKeys = false, sawFmtKeys = false, deadlineMillis)
    val confirmed                = sawModKeys && sawFmtKeys
    (confirmed, if confirmed then Array.emptyByteArray else consumed.result())

  private def disableKeyboardProtocol(terminal: Terminal, tier: KeyboardProtocolTier): Unit =
    val writer = terminal.writer()
    tier match
      case KeyboardProtocolTier.Kitty => writer.write(KittyPop)
      case KeyboardProtocolTier.ModifyOtherKeys =>
        writer.write(ModifyOtherKeysDisable)
        writer.write(FormatOtherKeysDisable)
      // Already reverted at negotiation time when confirmation failed -- nothing left to disable on exit.
      case KeyboardProtocolTier.Legacy => ()
