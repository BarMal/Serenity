package com.serenity.ui.theme.appearance

import java.lang.ProcessBuilder.Redirect
import java.util.Locale

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Resource}

/** Runs a query command; `None` when it could not be started or did not finish. */
type ProcessRunner = List[String] => IO[Option[ProcessOutput]]

object ProcessRunner:

  private val Timeout = 3.seconds

  /** A hung query must not stall the focus handler, so it is abandoned (and its process killed) after [[Timeout]]. */
  val system: ProcessRunner = command =>
    Resource
      .make(IO.blocking(startProcess(command)))(process => IO.blocking(process.destroyForcibly()).void)
      .use(process =>
        IO.interruptible {
          val stdout = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
          ProcessOutput(process.waitFor(), stdout)
        }
      )
      .timeout(Timeout)
      .map(Option.apply)
      .handleError(_ => None)

  private def startProcess(command: List[String]): Process =
    val process = new ProcessBuilder(command.asJava).redirectError(Redirect.DISCARD).start()
    process.getOutputStream.close()
    process

trait OsAppearanceDetector:
  /** Never fails: anything that goes wrong is [[OsAppearance.Unknown]]. */
  def detect: IO[OsAppearance]

object OsAppearanceDetector:

  def fixed(appearance: OsAppearance): OsAppearanceDetector = new OsAppearanceDetector:
    def detect: IO[OsAppearance] = IO.pure(appearance)

  lazy val system: OsAppearanceDetector = forOs(System.getProperty("os.name", ""), ProcessRunner.system)

  def forOs(osName: String, run: ProcessRunner): OsAppearanceDetector =
    val name = osName.toLowerCase(Locale.ROOT)
    val query: IO[OsAppearance] =
      if name.contains("mac") then macOs(run)
      else if name.contains("win") then windows(run)
      else if name.contains("linux") || name.contains("bsd") || name.contains("nix") then linux(run)
      else IO.pure(OsAppearance.Unknown)
    new OsAppearanceDetector:
      def detect: IO[OsAppearance] = query.handleError(_ => OsAppearance.Unknown)

  private def macOs(run: ProcessRunner): IO[OsAppearance] =
    for
      style    <- run(List("defaults", "read", "-g", "AppleInterfaceStyle"))
      contrast <- run(List("defaults", "read", "com.apple.universalaccess", "increaseContrast"))
    yield OsAppearanceParsers.macOs(style, contrast)

  private def windows(run: ProcessRunner): IO[OsAppearance] =
    for
      theme <- run(
        List(
          "reg",
          "query",
          "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
          "/v",
          "AppsUseLightTheme"
        )
      )
      contrast <- run(List("reg", "query", "HKCU\\Control Panel\\Accessibility\\HighContrast", "/v", "Flags"))
    yield OsAppearanceParsers.windows(theme, contrast)

  private def linux(run: ProcessRunner): IO[OsAppearance] =
    def gsettings(key: String) = run(List("gsettings", "get", "org.gnome.desktop.interface", key))
    for
      scheme <- gsettings("color-scheme")
      gtk    <- gsettings("gtk-theme")
    yield OsAppearanceParsers.linux(scheme, gtk)
