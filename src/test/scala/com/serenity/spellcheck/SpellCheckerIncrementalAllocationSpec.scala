package com.serenity.spellcheck

import java.lang.management.ManagementFactory

import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.IncrementalSpellFixture.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What re-checking after a one-character edit costs. The writer pauses after every few keystrokes, so the cost of a
  * refresh must follow the edit, not the document: a refresh that collects and re-reads a megabyte of prose each time
  * is felt as typing lag in a long manuscript.
  *
  * Measured the way [[com.serenity.state.manager.LspDocumentSyncPerformanceSpec]] measures allocation, on the calling
  * thread, which is the thread the pure refresh runs on.
  */
class SpellCheckerIncrementalAllocationSpec extends AnyFlatSpec with Matchers:

  // The balance the editor builds its ropes with: a megabyte is then about a thousand leaves, as in use.
  given Balance = Balance.default

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  /** The state analysed before, and the content after one character is typed into the middle of a document of `size`
    * characters. The misspellings are the same few at any size, so the diagnostics published do not grow with it.
    */
  private def checkedBefore(size: Int): (com.serenity.state.models.AppState, Rope) =
    val text   = "wurld teh wurld\n" + "the cat sat on the mat\n" * (size / 23)
    val before = Rope(text)
    (stateWith(before), before.insert(before.weight / 2, "x").getOrElse(before))

  private def allocatedByRefreshAfterOneEdit(size: Int): Option[Long] =
    allocationBean.map { bean =>
      val (state, after) = checkedBefore(size)
      val threadId       = Thread.currentThread().threadId()
      val edited_        = edited(state, after)
      (1 to 300).foreach(_ => refreshed(edited_))
      (1 to 5).map { _ =>
        val start = bean.getThreadAllocatedBytes(threadId)
        refreshed(edited_)
        bean.getThreadAllocatedBytes(threadId) - start
      }.min
    }

  "Re-checking after a one-character edit" should "cost the same for a 1 MB document as for a 10 KB one" in {
    (allocatedByRefreshAfterOneEdit(10 * 1024), allocatedByRefreshAfterOneEdit(1024 * 1024)) match
      case (Some(small), Some(large)) =>
        info(s"10 KB document allocated ${small}B, 1 MB document allocated ${large}B")
        withClue(s"10 KB document allocated ${small}B, 1 MB document allocated ${large}B: ") {
          large should be < small * 2L
        }
      case _ =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
end SpellCheckerIncrementalAllocationSpec
