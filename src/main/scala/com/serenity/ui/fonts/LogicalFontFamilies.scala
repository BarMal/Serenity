package com.serenity.ui.fonts

import java.awt.{Font, GraphicsEnvironment}
import java.util.concurrent.TimeUnit

import scala.util.Try

/** The physical family each of AWT's logical font names stands for on Linux, so the font layout measures with and the
  * font a surface draws with are the same face. AWT's logical fonts are composites it resolves in its own way, and a
  * surface that is not AWT cannot ask which face that is; naming one family for both makes measured equal drawn.
  *
  * Only Linux resolves, and only the five logical names. Elsewhere a logical name stays itself, because there the
  * composite is not its first physical family: measured on a Windows runner, `SansSerif` and `Arial` advance alike but
  * the composite's ascent is 12.06 against Arial's 10.86 (a 16-pixel line against 14), so handing AWT the physical
  * family would change every line height. Any other family, the bundled ones included, passes through, and so does a
  * logical name when fontconfig names no installed family for it.
  */
final class LogicalFontFamilies(
    osName: String,
    installedFamilies: Set[String],
    fontconfigBest: String => Option[String]
):

  // Fontconfig is asked once for each generic family, when the resolver is made: on Linux only, and never again.
  private val fontconfigFamilies: Map[String, Option[String]] =
    if LogicalFontFamilies.isLinux(osName) then
      LogicalFontFamilies.Generic.values.toSet.map(generic => generic -> fontconfigBest(generic)).toMap
    else Map.empty

  def resolve(family: String): String =
    LogicalFontFamilies
      .genericOf(family)
      .flatMap(generic =>
        fontconfigFamilies.get(generic).flatten.filter(name => installedFamilies.contains(name.toLowerCase))
      )
      .getOrElse(family)

object LogicalFontFamilies:

  private[fonts] val Generic = Map(
    "serif"       -> "serif",
    "sansserif"   -> "sans-serif",
    "dialog"      -> "sans-serif",
    "monospaced"  -> "monospace",
    "dialoginput" -> "monospace"
  )

  private val Windows = Map(
    "serif"       -> "Times New Roman",
    "sansserif"   -> "Arial",
    "dialog"      -> "Arial",
    "monospaced"  -> "Courier New",
    "dialoginput" -> "Courier New"
  )

  private val MacOs = Map(
    "serif"       -> "Times",
    "sansserif"   -> "Helvetica",
    "dialog"      -> "Helvetica",
    "monospaced"  -> "Courier",
    "dialoginput" -> "Courier"
  )

  private[fonts] def genericOf(family: String): Option[String] = Generic.get(family.toLowerCase)

  private[fonts] def isLinux(osName: String): Boolean = !osName.toLowerCase.contains("windows") && !isMac(osName)

  private def isMac(osName: String): Boolean = osName.toLowerCase.contains("mac")

  /** The family the JDK documents AWT's logical `logical` font as drawn in on Windows and macOS ("Physical and Logical
    * Fonts"); Linux has none, as there fontconfig decides. It is for the Skia side only, which must draw some face for
    * a logical name; AWT keeps the logical font there.
    */
  def documentedFamily(logical: String, osName: String): Option[String] =
    val key = logical.toLowerCase
    if osName.toLowerCase.contains("windows") then Windows.get(key)
    else if isMac(osName) then MacOs.get(key)
    else None

  /** This machine's resolver: the OS is asked once per generic family, and only about the five logical names. */
  lazy val system: LogicalFontFamilies =
    new LogicalFontFamilies(
      System.getProperty("os.name", ""),
      Try(GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toSet)
        .getOrElse(Set.empty)
        .map(_.toLowerCase),
      Fontconfig.bestFamily
    )

  val Logical: Set[String] = Set(Font.SERIF, Font.SANS_SERIF, Font.MONOSPACED, Font.DIALOG, Font.DIALOG_INPUT)

/** What `fc-match` ranks first for a generic family. A missing tool, a failure or a slow answer is no answer. */
object Fontconfig:

  private val TimeoutSeconds = 2L

  def parseFamily(output: String): Option[String] =
    output.linesIterator.map(_.trim).find(_.nonEmpty).map(_.takeWhile(_ != ',').trim).filter(_.nonEmpty)

  def bestFamily(generic: String): Option[String] =
    Try {
      val process = new ProcessBuilder("fc-match", "-f", "%{family[0]}", generic).redirectErrorStream(false).start()
      process.getOutputStream.close()
      if process.waitFor(TimeoutSeconds, TimeUnit.SECONDS) && process.exitValue() == 0 then
        parseFamily(new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8))
      else
        process.destroyForcibly()
        None
    }.toOption.flatten
