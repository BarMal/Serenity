package com.serenity.ui.layout

import java.text.BreakIterator
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A `BoundaryScratch` is one owner's break iterators, one set per thread: an iterator is stateful and not thread-safe,
  * and building one per use is what #1277 measured as too slow, so each thread keeps and reuses its own.
  */
class BoundaryScratchSpec extends AnyFlatSpec with Matchers:

  private def onAnotherThread[A](body: => A): A =
    val result = new AtomicReference[Option[A]](None)
    val thread = new Thread(() => result.set(Some(body)))
    thread.start()
    thread.join()
    result.get.getOrElse(fail("the other thread produced nothing"))

  "A scratch" should "hand one thread the same iterators every time" in {
    val scratch = new BoundaryScratch

    (scratch.characterBreaks eq scratch.characterBreaks) shouldBe true
    (scratch.lineBreaks eq scratch.lineBreaks) shouldBe true
  }

  it should "keep a separate set of iterators for each thread" in {
    val scratch = new BoundaryScratch
    val here    = (scratch.characterBreaks, scratch.lineBreaks)

    val there = onAnotherThread((scratch.characterBreaks, scratch.lineBreaks))

    ((here._1 eq there._1), (here._2 eq there._2)) shouldBe (false, false)
  }

  it should "share nothing with another scratch, even on the same thread" in {
    val first  = new BoundaryScratch
    val second = new BoundaryScratch

    ((first.characterBreaks eq second.characterBreaks), (first.lineBreaks eq second.lineBreaks)) shouldBe
      (false, false)
  }

  it should "break text by character and by line" in {
    val scratch = new BoundaryScratch
    scratch.characterBreaks.setText("a b")
    scratch.lineBreaks.setText("a b")

    (scratch.characterBreaks.next(), scratch.lineBreaks.next()) shouldBe (1, 2)
  }

  it should "not let one set of text disturb an iterator another scratch is reading" in {
    val first  = new BoundaryScratch
    val second = new BoundaryScratch
    first.characterBreaks.setText("abc")
    first.characterBreaks.next()

    second.characterBreaks.setText("xyzzy")

    first.characterBreaks.current() shouldBe 1
    first.characterBreaks.next() shouldBe 2
    BreakIterator.DONE should not be 2
  }
