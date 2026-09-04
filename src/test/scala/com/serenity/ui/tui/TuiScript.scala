package com.serenity.ui.tui

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cats.data.Kleisli
import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.{Event, MouseButton}
import com.serenity.state.models.{AppState, Buffer}
import com.serenity.ui.layout.ViewportSize

/** A scripted interaction with a running TUI session, composed as a for-comprehension.
  *
  * Every step is a value rather than a call on a receiver, so a scenario reads as a sequence of user actions and shared
  * openings (`openCommandPalette`, `saveAs(path)`) can be defined once and reused across specs -- see [[TuiScenarios]].
  * `TuiScript` is `Kleisli[IO, TuiSession, *]`, so `flatMap`, `traverse`, `replicateA` and the rest come from cats
  * rather than being reinvented here.
  */
type TuiScript[A] = Kleisli[IO, TuiSession, A]

object TuiScript:

  def apply[A](run: TuiSession => IO[A]): TuiScript[A] = Kleisli(run)

  def pure[A](value: A): TuiScript[A] = Kleisli.pure(value)

  val unit: TuiScript[Unit] = pure(())

  /** The session itself, for the rare step no primitive covers. */
  val session: TuiScript[TuiSession] = Kleisli.ask

  def liftIO[A](io: IO[A]): TuiScript[A] = Kleisli.liftF(io)

  // -- input --------------------------------------------------------------------------------------------------------

  /** Send a key's bytes and wait until every event they produced has been applied. */
  def press(key: TuiKey): TuiScript[Unit] = apply(_.feed(key))

  def pressAll(keys: TuiKey*): TuiScript[Unit] = apply(_.feedAll(keys))

  /** Type a run of text the way a terminal delivers a fast typist: one burst of bytes, decoded to one keystroke per
    * character.
    */
  def typeText(text: String): TuiScript[Unit] = press(TuiKeys.text(text))

  /** Type character by character, each one settled before the next is sent -- for scenarios where the state after every
    * individual keystroke matters (incremental search, a filter narrowing as it is typed).
    */
  def typeSlowly(text: String): TuiScript[Unit] =
    text.toList.traverse_(character => press(TuiKeys.char(character)))

  def ctrl(letter: Char): TuiScript[Unit]      = press(TuiKeys.ctrl(letter))
  def ctrlShift(letter: Char): TuiScript[Unit] = press(TuiKeys.ctrlShift(letter))

  val enter: TuiScript[Unit]      = press(TuiKeys.Enter)
  val escape: TuiScript[Unit]     = press(TuiKeys.Escape)
  val tab: TuiScript[Unit]        = press(TuiKeys.Tab)
  val backspace: TuiScript[Unit]  = press(TuiKeys.Backspace)
  val delete: TuiScript[Unit]     = press(TuiKeys.Delete)
  val arrowUp: TuiScript[Unit]    = press(TuiKeys.ArrowUp)
  val arrowDown: TuiScript[Unit]  = press(TuiKeys.ArrowDown)
  val arrowLeft: TuiScript[Unit]  = press(TuiKeys.ArrowLeft)
  val arrowRight: TuiScript[Unit] = press(TuiKeys.ArrowRight)
  val lineStart: TuiScript[Unit]  = press(TuiKeys.Home)
  val lineEnd: TuiScript[Unit]    = press(TuiKeys.End)
  val pageDown: TuiScript[Unit]   = press(TuiKeys.PageDown)
  val pageUp: TuiScript[Unit]     = press(TuiKeys.PageUp)

  def paste(text: String): TuiScript[Unit] = press(TuiKeys.paste(text))

  /** A full click: the press and the release a terminal reports as two separate mouse events. */
  def click(col: Int, row: Int, button: MouseButton = MouseButton.Primary): TuiScript[Unit] =
    press(TuiKeys.mousePress(col, row, button)) >> press(TuiKeys.mouseRelease(col, row, button))

  def moveMouse(col: Int, row: Int): TuiScript[Unit] = press(TuiKeys.mouseMove(col, row))

  def dragMouse(col: Int, row: Int): TuiScript[Unit] = press(TuiKeys.mouseDrag(col, row))

  val focusOut: TuiScript[Unit] = press(TuiKeys.FocusOut)
  val focusIn: TuiScript[Unit]  = press(TuiKeys.FocusIn)

  def resize(size: ViewportSize): TuiScript[Unit] = apply(_.resize(size))

  // -- observation --------------------------------------------------------------------------------------------------

  /** Paint a frame and snapshot what the terminal now shows. */
  val screen: TuiScript[TuiScreen] = apply(_.screen)

  /** The same, painted without the caret -- for asserting on content without the cursor's own cell in the way. */
  val screenWithoutCaret: TuiScript[TuiScreen] = apply(_.screenWithoutCaret)

  /** The frame once repainting has gone quiet -- the starting point for any assertion about emitted bytes. */
  val settledScreen: TuiScript[TuiScreen] = apply(_.settledScreen)

  val state: TuiScript[AppState] = apply(_.state)

  /** The buffer the editor is focused on, resolved from state the way the renderer resolves it. */
  def focusedBuffer(current: AppState): Option[Buffer] =
    current.focusedBufferId.flatMap(current.persisted.buffers.get)

  /** The text of the buffer currently being edited -- the state-side counterpart to reading it off the screen. */
  val documentText: TuiScript[Option[String]] =
    state.map(focusedBuffer(_).map(_.document.content.toString))

  val eventsApplied: TuiScript[Vector[Event]] = apply(_.eventsApplied)

  val allBytesWritten: TuiScript[String] = apply(_.allBytesWritten)

  val clipboardText: TuiScript[Option[String]] = apply(_.clipboard.readText)

  /** The on-disk content of a file in the session's own workspace -- how a save is verified. */
  def fileContent(name: String = TuiEnvironment.DefaultFileName): TuiScript[String] =
    apply(session => IO.blocking(Files.readString(session.workspace.resolve(name), StandardCharsets.UTF_8)))

  def workspaceFileExists(name: String): TuiScript[Boolean] =
    apply(session => IO.blocking(Files.exists(session.workspace.resolve(name))))

  def workspacePath(name: String): TuiScript[String] =
    apply(session => IO.pure(session.workspace.resolve(name).toString))

  // -- assertion ----------------------------------------------------------------------------------------------------

  /** Assert against a freshly rendered screen, attaching the whole grid to any failure so a red test shows the terminal
    * rather than a bare string mismatch.
    */
  def verify(label: String)(check: TuiScreen => Unit): TuiScript[Unit] =
    screen.flatMapF(current => attach(label, current)(check(current)))

  def verifyWithoutCaret(label: String)(check: TuiScreen => Unit): TuiScript[Unit] =
    screenWithoutCaret.flatMapF(current => attach(label, current)(check(current)))

  /** Assert against application state, with the screen attached on failure too -- state and screen disagreeing is
    * exactly the case where seeing both matters.
    */
  def verifyState(label: String)(check: AppState => Unit): TuiScript[Unit] =
    for
      current <- state
      shown   <- screen
      _       <- liftIO(attach(label, shown)(check(current)))
    yield ()

  private def attach(label: String, current: TuiScreen)(check: => Unit): IO[Unit] =
    IO(check).handleErrorWith { failure =>
      IO.raiseError(
        new AssertionError(
          s"""$label: ${failure.getMessage}
             |
             |${current.render}""".stripMargin,
          failure
        )
      )
    }

end TuiScript
