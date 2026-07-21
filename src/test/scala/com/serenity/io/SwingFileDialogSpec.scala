package com.serenity.io

import java.nio.file.Path

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
