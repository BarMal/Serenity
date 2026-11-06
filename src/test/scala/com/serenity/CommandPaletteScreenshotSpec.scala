package com.serenity

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The command palette through the real GUI renderer, in both bundled themes at 1x and 2x (#2577). With
  * `PALETTE_SCREENSHOTS_DIR=<dir>` set, each frame is also written there as a PNG so a visual change can be compared
  * before and after; the assertions hold either way.
  */
class CommandPaletteScreenshotSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val directory = sys.env.get("PALETTE_SCREENSHOTS_DIR").map(Path.of(_))

  private val queries = List("" -> "empty", "line" -> "line", "font size" -> "font-size", "export" -> "export")

  for
    theme <- List("dark", "light")
    scale <- List(1, 2)
  do
    s"The palette in the $theme theme at ${scale}x" should "paint a surface for every query" in
      queries.foreach { (query, slug) =>
        val environment =
          UiScenarioEnvironment(viewport = ViewportSize(120, 34), deviceScale = scale.toDouble, themeName = theme)
        val driver = UiScenarioDriver.create("palette", environment, artifactDirectory = directory).unsafeRunSync()
        driver.dispatch(ToggleCommandRunner).unsafeRunSync()
        query.foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
        val frame = driver.renderFrame(s"gui-$theme-${scale}x-$slug").unsafeRunSync()

        withClue(s"query '$query'") {
          frame.evidence.surfaceRects should not be empty
          frame.evidence.layoutViolations shouldBe empty
        }
      }
