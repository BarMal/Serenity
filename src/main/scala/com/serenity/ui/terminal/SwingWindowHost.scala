package com.serenity.ui.terminal

import cats.effect.{Deferred, IO, Resource}
import cats.syntax.all.*
import com.serenity.command.CommandId
import com.serenity.command.menu.MenuDispatch
import com.serenity.config.{MenuBarMode, PreferredWindowSize, WindowChromeMode}
import com.serenity.diagnostics.FrameTimings
import com.serenity.frontend.FrontendRuntime
import com.serenity.input.{InputHandler, InputRouter, SwingInputHandler}
import com.serenity.io.{FileDialog, FileDialogSelection, SwingFileDialog}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.ui.host.{InputSettings, WindowControl, WindowHost}
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.RenderTarget

/** The Swing window as a [[WindowHost]]: the window is both the render target and the control, and the AWT listeners,
  * menu bar and dialogs hang off its canvas. The menu bar needs the input handler the runtime builds later, so the
  * handler is passed to it through `menuInput`.
  */
final class SwingWindowHost private (val window: SwingWindow, menuInput: Deferred[IO, SwingInputHandler[IO, Event]])
    extends WindowHost:

  def target: RenderTarget   = window
  def control: WindowControl = window

  def inputHandler(router: InputRouter[IO, Event], settings: InputSettings): IO[InputHandler[IO]] =
    IO.pure(
      new SwingInputHandler[IO, Event](
        window.canvas,
        router,
        settings.metrics,
        settings.uiMetrics,
        settings.wheelScrollLines,
        settings.frameTimings,
        settings.imeDiagnostics
      )
    ).flatTap(menuInput.complete(_).void)
      .widen

  override def typeCharacter(char: Char): IO[Unit] =
    menuInput.get.flatMap(handler => IO(handler.submitTypedChar(char)))

  override def pressPageDown: IO[Unit] =
    menuInput.get.flatMap(handler => IO(handler.submitKey(KeyStrokeInfo(InputKey.PageDown, None, Set.empty))))

  override def toggleTheme: IO[Unit] =
    menuInput.get.flatMap(handler => IO(handler.submit(MenuDispatch.eventFor(CommandId("toggle-theme")))))

  def menus(mode: MenuBarMode): Option[FrontendRuntime.MenuHost => Resource[IO, Unit]] =
    Option.when(SwingMenuBar.enabledFor(System.getProperty("os.name", ""), mode))(
      SwingMenuBar.resource(window, menuInput.get, mode)
    )

  def fileDialog(selection: FileDialogSelection.Decision): FileDialog =
    SwingFileDialog(window.canvas, selection)

object SwingWindowHost:

  def resource(
    metrics: CellMetrics,
    chromeMetrics: CellMetrics,
    chromeMode: WindowChromeMode,
    preferredWindowSize: Option[PreferredWindowSize],
    frameTimings: FrameTimings,
    title: String
  ): Resource[IO, WindowHost] =
    SwingWindow
      .resource(metrics, chromeMetrics, chromeMode, preferredWindowSize, frameTimings, title)
      .evalMap(window =>
        Deferred[IO, SwingInputHandler[IO, Event]].map(menuInput => new SwingWindowHost(window, menuInput))
      )
