package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{MouseClick, MouseWheel, PanelInputEvent}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end scenario for #1953: every row of a long explorer can be reached and picked, by wheel and by keyboard,
  * through the real state pipeline and the Java2D renderer.
  */
class ExplorerScrollUiScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val explorerId = SurfaceId("explorer")
  private val root       = TestTemp.directory("explorer-scroll-")
  private val files      = (0 until 500).toList.map(index => Files.createFile(root.resolve(f"entry-$index%03d.md")))

  private val tree = DirectoryTreeData(
    root,
    entries = Map(root -> files.map(path => DirEntry(path, path.getFileName.toString, isDirectory = false)))
  )

  private def explorerDriver(name: String): UiScenarioDriver =
    val driver = UiScenarioDriver.create(name).unsafeRunSync()
    driver
      .updateState(state =>
        DockedPanelFixtures.dock(
          state,
          explorerId,
          SurfaceContent.DirectoryTree(tree, Some(root)),
          PanelPosition.Left,
          30
        )
      )
      .unsafeRunSync()
    driver

  private def explorerRect(evidence: ScenarioFrameEvidence): LayoutRect =
    evidence.surfaceRects.getOrElse(explorerId, fail(s"expected the explorer on screen: ${evidence.surfaceRects}"))

  /** The renderer draws glyph by glyph, so a row's text is its glyphs joined left to right. */
  private def drawnEntries(evidence: ScenarioFrameEvidence): List[(String, LayoutRect)] =
    val rect = explorerRect(evidence)
    evidence.drawnText
      .filter(text => rect.containsRect(text.bounds))
      .groupBy(_.bounds.y)
      .toList
      .sortBy(_._1)
      .map {
        case (row, glyphs) =>
          glyphs.sortBy(_.bounds.x).map(_.text).mkString.trim -> LayoutRect(rect.x, row, rect.width, 1)
      }
      .filter(_._1.startsWith("entry-"))

  private def drawnNames(evidence: ScenarioFrameEvidence): List[String] = drawnEntries(evidence).map(_._1)

  private def selectedPath(driver: UiScenarioDriver): Option[java.nio.file.Path] =
    driver.state
      .unsafeRunSync()
      .surfaceById(explorerId)
      .map(_.content)
      .collect { case explorer: SurfaceContent.DirectoryTree => explorer.selectedPath }
      .flatten

  private def wheelOverExplorer(driver: UiScenarioDriver, evidence: ScenarioFrameEvidence, lines: Int): Unit =
    val rect = explorerRect(evidence)
    driver.dispatch(MouseWheel(rect.x + rect.width / 2, rect.y + rect.height / 2, lines)).unsafeRunSync()

  "A 500-entry explorer" should "wheel down to its last entry and select it with a click" in {
    val driver = explorerDriver("explorer-500-wheel")
    val first  = driver.renderFrame("explorer-top").unsafeRunSync().evidence
    drawnNames(first) should not contain "entry-499.md"
    val shown = drawnNames(first).size
    shown should be > 3

    (1 to 500 / 3 + 1).foreach(_ => wheelOverExplorer(driver, first, 3))

    val bottom = driver.renderFrame("explorer-bottom").unsafeRunSync().evidence
    drawnNames(bottom).lastOption shouldBe Some("entry-499.md")
    drawnNames(bottom).size shouldBe shown + 1
    selectedPath(driver) shouldBe Some(root)

    val last = drawnEntries(bottom).lastOption.getOrElse(fail("expected the last entry drawn"))
    driver.dispatch(MouseClick(last._2.x + 2, last._2.y)).unsafeRunSync()

    selectedPath(driver) shouldBe Some(files.last)
    drawnNames(driver.renderFrame("explorer-clicked").unsafeRunSync().evidence) shouldBe drawnNames(bottom)
  }

  it should "reach the end with End, page back with PageUp, and return to the top with Home" in {
    val driver = explorerDriver("explorer-500-keys")
    driver
      .updateState(state => state.copy(persisted = state.persisted.copy(focus = Focus.Surface(explorerId))))
      .unsafeRunSync()
    val shown = drawnNames(driver.renderFrame("keys-top").unsafeRunSync().evidence).size

    driver.dispatch(PanelInputEvent.Last).unsafeRunSync()
    val atEnd = driver.renderFrame("keys-end").unsafeRunSync().evidence
    selectedPath(driver) shouldBe Some(files.last)
    drawnNames(atEnd).lastOption shouldBe Some("entry-499.md")

    driver.dispatch(PanelInputEvent.Page(-1)).unsafeRunSync()
    val pagedBack = selectedPath(driver).getOrElse(fail("expected a selection"))
    drawnNames(driver.renderFrame("keys-paged").unsafeRunSync().evidence) should contain(pagedBack.getFileName.toString)
    files.indexOf(pagedBack) should be < files.size - 1

    driver.dispatch(PanelInputEvent.First).unsafeRunSync()
    selectedPath(driver) shouldBe Some(root)
    drawnNames(driver.renderFrame("keys-home").unsafeRunSync().evidence).headOption shouldBe Some("entry-000.md")
  }
