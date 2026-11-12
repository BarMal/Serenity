package com.serenity.perf

import com.serenity.diagnostics.FrameCounter
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.ColumnPagingFixture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A frame that keeps rows clips each draw near them off the kept pixels, and for that works out the rectangles it may
  * paint in. The rows a draw lies in are the same for every run in a row, so a frame should work them out once for each
  * row it draws, not once for each run and fill in it. What gets painted is compared with a cold frame in
  * [[KeptClipFramesSpec]], so that these counts are of the Skia path on every platform.
  */
class KeptClipSweepsSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  for scale <- List(1.0, 2.0) do
    s"A typing frame at ${scale}x" should "sweep for clip pieces at most once for each row it draws" in {
      val session = Session(ColumnPagingFixture.document, scale)
      session.markModified()
      session.placeCursor(3, 30)
      val frames = (1 to 6).map { n =>
        session(InsertChar('w'), s"type $n")
        session.lastFrameCounts
      }
      frames.foreach { counts =>
        counts(FrameCounter.RowsDrawn) should be > 0L
        counts(FrameCounter.ClipSweeps) should be <= counts(FrameCounter.RowsDrawn)
      }
    }
