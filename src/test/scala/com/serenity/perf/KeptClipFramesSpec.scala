package com.serenity.perf

import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.ColumnPagingFixture

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a typing frame that keeps rows paints is what a cold frame paints, in the column-paging document
  * [[KeptClipSweepsSpec]] counts the sweeps of. The two are separate so that the counting runs on Skia everywhere.
  */
class KeptClipFramesSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  for scale <- List(1.0, 2.0) do
    s"A typing frame at ${scale}x" should "draw what a cold frame draws at every step" in {
      val session = Session(ColumnPagingFixture.document, scale)
      session.markModified()
      session.placeCursor(3, 30)
      (1 to 6).foreach(n => session(InsertChar('w'), s"type $n"))
      session.mismatches shouldBe empty
    }
