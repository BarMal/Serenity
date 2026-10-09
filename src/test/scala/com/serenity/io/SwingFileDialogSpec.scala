package com.serenity.io

import java.lang.reflect.InvocationTargetException
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.SwingUtilities

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingFileDialogSpec extends AnyFlatSpec with Matchers:

  "SwingFileDialog" should "prefer the modern Windows dialog when a native owner is available" in
    SwingFileDialog
      .preferredBackend(hasNativeOwner = true, osName = "Windows 11")
      .shouldBe(SwingFileDialog.Backend.WindowsModern)

  it should "prefer the AWT native dialog on non-Windows systems when a native owner is available" in
    SwingFileDialog
      .preferredBackend(hasNativeOwner = true, osName = "Mac OS X")
      .shouldBe(SwingFileDialog.Backend.Native)

  it should "fall back to JFileChooser when no native owner is available" in
    SwingFileDialog
      .preferredBackend(hasNativeOwner = false, osName = "Windows 11")
      .shouldBe(SwingFileDialog.Backend.SwingChooser)

  it should "normalize native dialog selections from directory and file parts" in {
    val selected = SwingFileDialog.normalizeNativeSelection(Path.of("tmp", "drafts", "..").toString, "notes.md")

    selected.shouldBe(Some(Path.of("tmp", "notes.md").normalize()))
  }

  it should "treat a missing native selection as cancellation" in
    SwingFileDialog.normalizeNativeSelection(Path.of("tmp").toString, null).shouldBe(None)

  it should "normalize JFileChooser selections" in {
    val selected = SwingFileDialog.normalizeSwingSelection(Path.of("tmp", "drafts", "..", "notes.md").toFile)

    selected.shouldBe(Some(Path.of("tmp", "notes.md").normalize()))
  }

  it should "treat a missing JFileChooser selection as cancellation" in
    SwingFileDialog.normalizeSwingSelection(null).shouldBe(None)

  "Choosing a folder" should "use the modern Windows dialog with a native owner" in
    SwingFileDialog
      .preferredFolderBackend(hasNativeOwner = true, osName = "Windows 11")
      .shouldBe(SwingFileDialog.Backend.WindowsModern)

  it should "use the AWT dialog on macOS, which can switch to directories" in
    SwingFileDialog
      .preferredFolderBackend(hasNativeOwner = true, osName = "Mac OS X")
      .shouldBe(SwingFileDialog.Backend.Native)

  it should "use JFileChooser on Linux, where the AWT dialog cannot pick a directory" in
    SwingFileDialog
      .preferredFolderBackend(hasNativeOwner = true, osName = "Linux")
      .shouldBe(SwingFileDialog.Backend.SwingChooser)

  it should "use JFileChooser on any system without a native owner" in
    List("Windows 11", "Mac OS X", "Linux").foreach { osName =>
      SwingFileDialog
        .preferredFolderBackend(hasNativeOwner = false, osName = osName)
        .shouldBe(SwingFileDialog.Backend.SwingChooser)
    }

  it should "ask the Windows Common Item Dialog for folders with FOS_PICKFOLDERS and keep its other options" in {
    SwingFileDialog.PickFolders shouldBe 0x20
    val options = SwingFileDialog.windowsDialogOptions(current = 0x2, pickFolders = true)

    (options & SwingFileDialog.PickFolders) shouldBe SwingFileDialog.PickFolders
    (options & SwingFileDialog.ForceFileSystem) shouldBe SwingFileDialog.ForceFileSystem
    (options & 0x2) shouldBe 0x2
  }

  it should "leave FOS_PICKFOLDERS off when picking a file" in {
    val options = SwingFileDialog.windowsDialogOptions(current = 0x2, pickFolders = false)

    (options & SwingFileDialog.PickFolders) shouldBe 0
    (options & SwingFileDialog.ForceFileSystem) shouldBe SwingFileDialog.ForceFileSystem
  }

  it should "read a folder picked in the macOS AWT dialog as its parent directory and name" in
    SwingFileDialog
      .normalizeNativeSelection(Path.of("tmp", "projects").toString, "novel")
      .shouldBe(Some(Path.of("tmp", "projects", "novel")))

  "Running on the event thread" should "return the value the body computed" in
    SwingFileDialog
      .runOnEventThread(onDispatchThread = false, _.run())(Some(Path.of("a")))
      .shouldBe(Right(Some(Path.of("a"))))

  it should "hand back the exception the body raised, not the task's wrapper" in {
    val boom = IllegalStateException("boom")

    SwingFileDialog
      .runOnEventThread(onDispatchThread = false, _.run())(throw boom)
      .shouldBe(Left(boom))
  }

  it should "unwrap the invoker's InvocationTargetException to its cause" in {
    val boom = IllegalStateException("boom")

    SwingFileDialog
      .runOnEventThread[Int](onDispatchThread = false, _ => throw InvocationTargetException(boom))(1)
      .shouldBe(Left(boom))
  }

  it should "run inline, without the invoker, when already on the dispatch thread" in {
    val invocations = AtomicInteger(0)

    val result = SwingFileDialog.runOnEventThread(onDispatchThread = true, _ => invocations.incrementAndGet(): Unit)(7)

    (result, invocations.get()).shouldBe((Right(7), 0))
  }

  it should "use the invoker exactly once when off the dispatch thread" in {
    val invocations = AtomicInteger(0)

    val result = SwingFileDialog.runOnEventThread(
      onDispatchThread = false,
      task =>
        invocations.incrementAndGet()
        task.run()
    )(7)

    (result, invocations.get()).shouldBe((Right(7), 1))
  }

  it should "run the body on the real dispatch thread and return its value" in
    SwingFileDialog.onEventThread(SwingUtilities.isEventDispatchThread).unsafeRunSync().shouldBe(true)

  it should "raise the body's own exception from the effect" in {
    val boom = IllegalStateException("dialog failed")

    SwingFileDialog.onEventThread[Int](throw boom).attempt.unsafeRunSync().shouldBe(Left(boom))
  }
