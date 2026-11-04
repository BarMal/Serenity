package com.serenity.testkit

import java.awt.{GraphicsEnvironment, Window}
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFrame, SwingUtilities}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DisplayExclusiveSpec extends AnyFlatSpec with Matchers:

  private val Patience = 30_000_000_000L

  private def withDisplay(): Unit =
    assume(!GraphicsEnvironment.isHeadless, "The check opens a real window, which needs a display")

  private def onSwing(action: => Unit): Unit = SwingUtilities.invokeAndWait(() => action)

  private def showingWindows: List[Window] = Window.getWindows.filter(_.isShowing).toList

  private def awaitBlocked(thread: Thread): Boolean =
    val deadline = System.nanoTime() + Patience

    @annotation.tailrec
    def poll(): Boolean =
      if thread.getState == Thread.State.BLOCKED then true
      else if !thread.isAlive || System.nanoTime() >= deadline then false
      else
        Thread.onSpinWait()
        poll()

    poll()

  "DisplayExclusive.withShowingWindowsAfter" should "return the body's result" in {
    DisplayExclusive.withShowingWindowsAfter(21 * 2)._1 shouldBe 42
  }

  it should "list a window the body left showing, and none once it is closed" in {
    withDisplay()
    val frame = new JFrame("left open")
    try
      val (_, open) = DisplayExclusive.withShowingWindowsAfter(onSwing(frame.setVisible(true)))
      open should contain(frame)
    finally onSwing(frame.dispose())
    DisplayExclusive.withShowingWindowsAfter(())._2 shouldBe empty
  }

  it should "wait for a window another lock-holder has up, and see none once that holder is done" in {
    withDisplay()
    val windowUp = new CountDownLatch(1)
    val release  = new CountDownLatch(1)
    val seen     = new AtomicReference[Option[List[Window]]](None)
    val holder = new Thread(
      new Runnable:
        def run(): Unit =
          DisplayExclusive {
            val frame = new JFrame("held by another spec")
            onSwing(frame.setVisible(true))
            windowUp.countDown()
            release.await()
            onSwing(frame.dispose())
          }
    )
    val checker = new Thread(
      new Runnable:
        def run(): Unit = seen.set(Some(DisplayExclusive.withShowingWindowsAfter(())._2))
    )
    holder.start()
    windowUp.await()
    showingWindows should not be empty
    checker.start()
    val blocked = awaitBlocked(checker)
    val early   = seen.get()
    release.countDown()
    holder.join()
    checker.join()
    withClue("the check must wait on the lock while the other spec's window is up: ")(blocked shouldBe true)
    early shouldBe None
    seen.get() shouldBe Some(Nil)
  }
