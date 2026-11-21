package com.serenity.testkit

import java.awt.{GraphicsEnvironment, Window}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
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

  /** The window is up only while this thread holds the display, as a spec that shows a window must hold it: shown
    * before the lock is taken, it is on screen while another spec holds the lock and lists the open windows.
    */
  private def windowClosedLateByTheBody(): Unit =
    DisplayExclusive {
      val frame   = new JFrame("closing late")
      val release = new CountDownLatch(1)
      val timer   = Executors.newSingleThreadScheduledExecutor()
      onSwing(frame.setVisible(true))
      try
        DisplayExclusive {
          SwingUtilities.invokeLater { () =>
            release.await()
            frame.dispose()
          }
          timer.schedule((() => release.countDown()): Runnable, 200, TimeUnit.MILLISECONDS): Unit
        }
        frame.isShowing shouldBe false
      finally
        release.countDown()
        timer.shutdownNow(): Unit
        onSwing(frame.dispose())
    }

  "DisplayExclusive" should "have the event thread caught up when it lets go, so a window the body asked to close late is gone" in {
    withDisplay()
    windowClosedLateByTheBody()
  }

  it should "keep a spec's own window out of sight of one listing the windows under the lock" in {
    withDisplay()
    val holding = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val listed  = new AtomicReference[List[Window]](Nil)
    val failure = new AtomicReference[Option[Throwable]](None)
    val holder = new Thread(
      new Runnable:
        def run(): Unit =
          listed.set(DisplayExclusive.withShowingWindowsAfter {
            holding.countDown()
            release.await()
          }._2)
    )
    val scenario = new Thread(
      new Runnable:
        def run(): Unit =
          try windowClosedLateByTheBody()
          catch case failed: Throwable => failure.set(Some(failed))
    )
    holder.start()
    holding.await()
    scenario.start()
    val waiting = awaitBlocked(scenario)
    release.countDown()
    holder.join()
    scenario.join()
    withClue("the scenario must wait on the lock the other spec holds: ")(waiting shouldBe true)
    failure.get() shouldBe None
    listed.get() shouldBe empty
  }

  "DisplayExclusive.withShowingWindowsAfter" should "return the body's result" in {
    DisplayExclusive.withShowingWindowsAfter(21 * 2)._1 shouldBe 42
  }

  it should "list a window the body left showing, and none once it is closed" in {
    withDisplay()
    val frame = new JFrame("left open")
    DisplayExclusive {
      try
        val (_, open) = DisplayExclusive.withShowingWindowsAfter(onSwing(frame.setVisible(true)))
        open should contain(frame)
      finally onSwing(frame.dispose())
    }
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
