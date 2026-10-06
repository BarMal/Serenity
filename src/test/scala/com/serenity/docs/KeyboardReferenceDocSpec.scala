package com.serenity.docs

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeyboardReferenceDocSpec extends AnyFlatSpec with Matchers:

  "docs/user/keyboard.md" should "hold the default binding table generated from HotkeyConfig for both platform families" in {
    withClue(s"${KeyboardReferenceTable.Document} is missing: ") {
      Files.exists(KeyboardReferenceTable.Document) shouldBe true
    }
    val current = new String(Files.readAllBytes(KeyboardReferenceTable.Document), StandardCharsets.UTF_8)
    withClue(
      "docs/user/keyboard.md is out of date. Run `sbt \"Test/runMain com.serenity.docs.KeyboardReferenceTable\"` and commit the result: "
    ) {
      KeyboardReferenceTable.spliced(current) shouldBe Right(current)
    }
  }

  it should "list the command palette as Ctrl+P for Linux and Windows and Cmd+P for macOS" in {
    val table = KeyboardReferenceTable.table
    table should include("| Command palette | `command_palette` | `Ctrl+P`, `Ctrl+Ctrl` | `Cmd+P`, `Cmd+Cmd` |")
  }
