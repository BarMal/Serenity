package com.serenity

import java.awt.Font

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Panels the frame keeps are never a source of stale pixels where the window ends partway through a line: the cell
  * grid does not divide the window, so a row's ink and a panel's bleed land on pixel rows a whole-cell reading of the
  * damage would miss (the case a Windows runner found for the Skia copy-out).
  */
abstract class UndamagedPanelGeometrySpec(codeSize: Int, windowHeight: Int)
    extends AnyFlatSpec
    with Matchers
    with PersistedFrameFixture:

  override protected lazy val codeFont: Font = Font(Font.MONOSPACED, Font.PLAIN, codeSize)
  override protected lazy val logicalH: Int  = windowHeight

  private val second = BufferId(2)

  private def start: AppState =
    val base = stateOf(codeLines.mkString("\n"), CursorPosition(10, 0))
    base.copy(persisted =
      base.persisted.copy(
        buffers = base.persisted.buffers.updated(second, Buffer.fromString(second, "second file")),
        bufferOrder = base.persisted.bufferOrder :+ second
      )
    )

  private def script(from: AppState): List[AppState] =
    val typed = List.iterate(from, 4)(state => typedAt(state, codeLines, 12, "x")).drop(1)
    val scrolled = List
      .iterate(typed.last, 5)(state => scrolledTo(state, state.persisted.buffers(bufferId).viewport.topLine + 3))
      .drop(1)
    val backUp = List
      .iterate(scrolled.last, 4)(state =>
        scrolledTo(state, (state.persisted.buffers(bufferId).viewport.topLine - 2).max(0))
      )
      .drop(1)
    typed ++ scrolled ++ backUp ++ List.iterate(backUp.last, 3)(state => typedAt(state, codeLines, 14, "y")).drop(1)

  s"A window ${windowHeight}px high drawn in a $codeSize pt code font" should
    "show what a fresh frame shows after typing, scrolling and typing again, on both images" in {
      val session = new Session()
      val first   = start
      val _       = session.settle(first)
      val _ = script(first).foldLeft(first) { (before, after) =>
        val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))
        withClue(s"${codeSize}pt ${windowHeight}px: ")(differingPixels(drawn, cold(after)) shouldBe empty)
        after
      }
    }

class UndamagedPanelGeometry10ptSpec extends UndamagedPanelGeometrySpec(10, 600)
class UndamagedPanelGeometry12ptSpec extends UndamagedPanelGeometrySpec(12, 613)
class UndamagedPanelGeometry15ptSpec extends UndamagedPanelGeometrySpec(15, 613)
