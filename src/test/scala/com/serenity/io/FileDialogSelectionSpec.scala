package com.serenity.io

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.config.FileDialogMode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileDialogSelectionSpec extends AnyFlatSpec with Matchers:

  private def decide(mode: FileDialogMode, os: String, schemaInstalled: Boolean) =
    FileDialogSelection.decide(mode, os, schemaInstalled)

  "auto mode on Linux" should "use the native dialog when the GTK FileChooser schema is installed" in {
    decide(FileDialogMode.Auto, "Linux", schemaInstalled = true).useNative shouldBe true
  }

  it should "use the Swing chooser when the schema is missing, because GLib would abort the process" in {
    val decision = decide(FileDialogMode.Auto, "Linux", schemaInstalled = false)
    decision.useNative shouldBe false
    decision.reason should include(GtkFileChooserSchema.SchemaId)
  }

  "auto mode off Linux" should "use the native dialog whatever the schema probe says" in {
    decide(FileDialogMode.Auto, "Windows 11", schemaInstalled = false).useNative shouldBe true
    decide(FileDialogMode.Auto, "Mac OS X", schemaInstalled = false).useNative shouldBe true
  }

  "native mode" should "use the native dialog even without the schema, and say so" in {
    val decision = decide(FileDialogMode.Native, "Linux", schemaInstalled = false)
    decision.useNative shouldBe true
    decision.reason should include("ui.file_dialog = native")
  }

  "swing mode" should "use the Swing chooser on every platform" in
    List("Linux", "Windows 11", "Mac OS X").foreach { os =>
      decide(FileDialogMode.Swing, os, schemaInstalled = true).useNative shouldBe false
    }

  "the schema probe" should "be consulted only when auto mode runs on Linux" in {
    val probes = AtomicInteger(0)
    def probe: Boolean =
      probes.incrementAndGet()
      true
    FileDialogSelection.decide(FileDialogMode.Native, "Linux", probe)
    FileDialogSelection.decide(FileDialogMode.Swing, "Linux", probe)
    FileDialogSelection.decide(FileDialogMode.Auto, "Windows 11", probe)
    probes.get shouldBe 0
    FileDialogSelection.decide(FileDialogMode.Auto, "Linux", probe)
    probes.get shouldBe 1
  }

  "the backend preference" should "take the Swing chooser whenever native dialogs are ruled out" in {
    SwingFileDialog.preferredBackend(hasNativeOwner = true, "Linux", nativeDialogs = false) shouldBe
      SwingFileDialog.Backend.SwingChooser
    SwingFileDialog.preferredBackend(hasNativeOwner = true, "Windows 11", nativeDialogs = false) shouldBe
      SwingFileDialog.Backend.SwingChooser
    SwingFileDialog.preferredFolderBackend(hasNativeOwner = true, "Mac OS X", nativeDialogs = false) shouldBe
      SwingFileDialog.Backend.SwingChooser
  }

  it should "keep the native backends when they are allowed" in {
    SwingFileDialog.preferredBackend(hasNativeOwner = true, "Linux", nativeDialogs = true) shouldBe
      SwingFileDialog.Backend.Native
    SwingFileDialog.preferredFolderBackend(hasNativeOwner = true, "Mac OS X", nativeDialogs = true) shouldBe
      SwingFileDialog.Backend.Native
  }

  "the open, save and folder actions" should "reach a native dialog only through the backend selection" in {
    val source = Files.readString(Path.of("src/main/scala/com/serenity/io/SwingFileDialog.scala"))
    val members = source
      .split("\n(?=  (?:private |private\\[io\\] )?def )")
      .flatMap(chunk => "def (\\w+)".r.findFirstMatchIn(chunk).map(_.group(1) -> chunk))
      .toMap

    def member(name: String): String = members.getOrElse(name, fail(s"SwingFileDialog has no member $name"))

    List("choose", "chooseFolder").foreach { name =>
      withClue(s"$name: ") {
        member(name) should (include("selection.useNative") and (include("preferredBackend(") or include(
          "preferredFolderBackend("
        )))
      }
    }
    member("apply") should (include("choose(parent, selection") and include("chooseFolder(parent, selection"))
    member("apply") should not include "chooseWithNativeDialog"
    member("apply") should not include "AwtFileDialog("

    val reachingNative = members.collect { case (name, text) if text.contains("AwtFileDialog(") => name }.toSet
    reachingNative shouldBe Set("chooseWithNativeDialog")
    val callers = members.collect { case (name, text) if text.contains("chooseWithNativeDialog(") => name }.toSet
    callers shouldBe Set("choose", "chooseFolder", "chooseWithModernWindowsDialog", "chooseWithNativeDialog")
  }
