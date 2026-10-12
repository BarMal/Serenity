package com.serenity.io

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.rope.Balance
import com.serenity.state.models.{BufferId, IndentationSettings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A file opened in a directory with an `.editorconfig` is indented as it says, below a choice and above detection
  * (#1996).
  */
class FileManagerEditorConfigSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val global = IndentationSettings(tabSize = 4, insertSpaces = true)

  private val indentedByTwo = "def f():\n  a\n  b\n"

  private def project(config: String, name: String, text: String): Path =
    val directory = TestTemp.directory("file-manager-editorconfig")
    val _         = Files.writeString(directory.resolve(".editorconfig"), config)
    Files.writeString(directory.resolve(name), text)

  "Opening a file" should "indent it as the .editorconfig above it says" in {
    val path   = project("root = true\n[*.py]\nindent_style = tab\ntab_width = 8\n", "a.py", "x\n")
    val opened = FileManager().loadFile(path, BufferId(1)).unsafeRunSync()

    opened.effectiveIndentation(global) shouldBe IndentationSettings(tabSize = 8, insertSpaces = false)
  }

  it should "prefer the .editorconfig to what the text shows" in {
    val path   = project("root = true\n[*]\nindent_style = space\nindent_size = 3\n", "a.py", "def f():\n\ta\n\tb\n")
    val opened = FileManager().loadFile(path, BufferId(1)).unsafeRunSync()

    opened.effectiveIndentation(global) shouldBe IndentationSettings(tabSize = 3, insertSpaces = true)
  }

  it should "fall back to detection for what the .editorconfig leaves unsaid" in {
    val path   = project("root = true\n[*]\nindent_style = space\n", "a.py", indentedByTwo)
    val opened = FileManager().loadFile(path, BufferId(1)).unsafeRunSync()

    opened.effectiveIndentation(global) shouldBe IndentationSettings(tabSize = 2, insertSpaces = true)
  }

  it should "prefer a choice to the .editorconfig" in {
    val path   = project("root = true\n[*]\nindent_size = 3\n", "a.py", indentedByTwo)
    val opened = FileManager().loadFile(path, BufferId(1)).unsafeRunSync()
    val chosen = opened.copy(document =
      opened.document.copy(indentation =
        opened.document.indentation.withChosen(Some(IndentationSettings(tabSize = 6, insertSpaces = false)))
      )
    )

    chosen.effectiveIndentation(global) shouldBe IndentationSettings(tabSize = 6, insertSpaces = false)
  }

  it should "ignore a section that does not match the file" in {
    val path   = project("root = true\n[*.rb]\nindent_size = 3\n", "a.py", indentedByTwo)
    val opened = FileManager().loadFile(path, BufferId(1)).unsafeRunSync()

    opened.document.indentation.configured.isEmpty shouldBe true
  }

  "Reloading a buffer" should "pick up an .editorconfig that changed meanwhile" in {
    val path    = project("root = true\n[*]\nindent_size = 3\n", "a.py", "x\n")
    val manager = FileManager()
    val opened  = manager.loadFile(path, BufferId(1)).unsafeRunSync()
    val _       = Files.writeString(path.resolveSibling(".editorconfig"), "root = true\n[*]\nindent_size = 12\n")

    manager.reloadBuffer(opened).unsafeRunSync().effectiveIndentation(global).tabSize shouldBe 12
  }

  "Saving a buffer under another name" should "indent it as the new location says" in {
    val path    = project("root = true\n[*.py]\nindent_size = 3\n[*.txt]\nindent_size = 7\n", "a.py", "x\n")
    val manager = FileManager()
    val opened  = manager.loadFile(path, BufferId(1)).unsafeRunSync()

    val saved = manager.saveBuffer(opened, path.resolveSibling("b.txt")).unsafeRunSync()

    saved.effectiveIndentation(global).tabSize shouldBe 7
  }

end FileManagerEditorConfigSpec
