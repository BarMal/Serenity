package com.serenity.ui.tui

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import com.serenity.config.AppConfig
import com.serenity.ui.layout.ViewportSize

/** Terminal sizes worth testing against, named rather than spelled out per spec so a scenario says what kind of
  * terminal it is about.
  *
  * [[Default]] is a full-screen terminal on an ordinary high-DPI laptop: a 3000x2000 panel at 2x scaling is 1500x1000
  * points, which at a typical 8x17-point monospace cell is roughly 187 columns by 58 rows. Testing at that size rather
  * than the traditional 80x24 keeps chrome, gutter and content from colliding by coincidence, which is exactly the kind
  * of bug a cramped fixture hides.
  */
object TuiViewport:
  val Default: ViewportSize    = ViewportSize(200, 56)
  val HalfScreen: ViewportSize = ViewportSize(100, 56)
  val Wide: ViewportSize       = ViewportSize(240, 64)
  val Small: ViewportSize      = ViewportSize(80, 24)
  val Tiny: ViewportSize       = ViewportSize(40, 10)

/** What a session starts as: how big the terminal is, what configuration it runs on, and what (if anything) is already
  * open in it. Everything is a temporary file under the session's own workspace, so a spec never touches the
  * developer's real config, session state or files.
  */
final case class TuiEnvironment(
    viewport: ViewportSize = TuiViewport.Default,
    config: AppConfig = AppConfig.default,
    file: Option[TuiEnvironment.SourceFile] = None,
    useOsc52Clipboard: Boolean = true,
    escDeadline: FiniteDuration = TerminalInputHandler.EscDisambiguationDeadline
):
  def withViewport(size: ViewportSize): TuiEnvironment = copy(viewport = size)

  /** How long a lone `ESC` is held before it resolves to a bare Escape. Set it to zero to prove that decoding an escape
    * sequence does not depend on that deadline, only on whether the rest of the sequence actually arrived.
    */
  def withEscDeadline(deadline: FiniteDuration): TuiEnvironment = copy(escDeadline = deadline)

  def withConfig(update: AppConfig => AppConfig): TuiEnvironment = copy(config = update(config))

  def withFile(content: String, name: String = TuiEnvironment.DefaultFileName): TuiEnvironment =
    copy(file = Some(TuiEnvironment.SourceFile(name, content)))

  def withoutFile: TuiEnvironment = copy(file = None)

  private[tui] def materialise(workspace: Path): IO[Option[Path]] =
    file.traverseIO { source =>
      IO.blocking {
        val path = workspace.resolve(source.name)
        Files.writeString(path, source.content, StandardCharsets.UTF_8)
      }
    }

  extension (option: Option[TuiEnvironment.SourceFile])
    private def traverseIO(write: TuiEnvironment.SourceFile => IO[Path]): IO[Option[Path]] =
      option.fold(IO.pure(Option.empty[Path]))(source => write(source).map(Some.apply))

object TuiEnvironment:

  final case class SourceFile(name: String, content: String)

  val DefaultFileName = "scratch.md"

  /** An empty document open in a full-screen terminal: the starting point for anything about editing. */
  val default: TuiEnvironment = TuiEnvironment(file = Some(SourceFile(DefaultFileName, "")))

  /** No file argument at all, so the session opens on the start page -- what `serenity --tui` alone does. */
  val startPage: TuiEnvironment = TuiEnvironment(file = None)

  def withFile(content: String, name: String = DefaultFileName): TuiEnvironment =
    default.withFile(content, name)

  /** A document of `lines` numbered lines, for scrolling and large-content scenarios. */
  def withLines(lines: Int, prefix: String = "line"): TuiEnvironment =
    withFile((0 until lines).map(index => s"$prefix $index").mkString("\n"))
end TuiEnvironment
