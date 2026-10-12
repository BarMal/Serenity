package com.serenity.io

import java.nio.file.Path

import com.serenity.state.models.IndentationHint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Parsing `.editorconfig` text and settling its properties for a path (#1996). */
class EditorConfigSpec extends AnyFlatSpec with Matchers:

  private def layer(directory: String, text: String): EditorConfig.Layer =
    EditorConfig.Layer(Path.of(directory), EditorConfig.parse(text))

  private def properties(path: String, layers: EditorConfig.Layer*): Map[String, String] =
    EditorConfig.propertiesFor(layers.toList, Path.of(path))

  "Parsing" should "read sections and properties, ignoring comments and blank lines" in {
    val file = EditorConfig.parse("# top\nroot = true\n\n; a section\n[*.py]\nindent_size = 4\n")

    file.root shouldBe true
    file.sections.map(_.properties) shouldBe Vector(Vector("indent_size" -> "4"))
  }

  it should "lower-case keys and values" in {
    val file = EditorConfig.parse("[*]\nIndent_Style = SPACE\n")

    file.sections.map(_.properties) shouldBe Vector(Vector("indent_style" -> "space"))
  }

  it should "accept a colon as the separator, any spacing and a byte order mark" in {
    val file = EditorConfig.parse("﻿root:true\n[*]\n  indent_size   =   2  \r\nindent_style:tab\r\n")

    file.root shouldBe true
    file.sections.map(_.properties) shouldBe Vector(Vector("indent_size" -> "2", "indent_style" -> "tab"))
  }

  it should "take root only from before the first section" in {
    EditorConfig.parse("[*]\nroot = true\n").root shouldBe false
    EditorConfig.parse("ROOT = TRUE\n").root shouldBe true
    EditorConfig.parse("root = false\n").root shouldBe false
  }

  it should "keep a bracket inside a section name" in {
    EditorConfig.parse("[[ab].c]\nx = 1\n").sections.map(_.glob.matches("a.c")) shouldBe Vector(true)
  }

  "Settling properties" should "let a later section override an earlier one" in {
    val file = layer("/project", "[*]\nindent_size = 2\n[*.scala]\nindent_size = 3\n[*.py]\nindent_size = 9\n")

    properties("/project/a.scala", file) shouldBe Map("indent_size" -> "3")
  }

  it should "let a nearer file override a farther one" in {
    val farther = layer("/project", "[*]\nindent_size = 2\nindent_style = space\n")
    val nearer  = layer("/project/sub", "[*]\nindent_size = 8\n")

    properties("/project/sub/a.txt", farther, nearer) shouldBe Map("indent_size" -> "8", "indent_style" -> "space")
  }

  it should "match section names against the path relative to their own file" in {
    val file = layer("/project/sub", "[/a.txt]\nx = 1\n[sub/a.txt]\nx = 2\n[b/*.txt]\ny = 3\n")

    properties("/project/sub/a.txt", file) shouldBe Map("x" -> "1")
    properties("/project/sub/b/c.txt", file) shouldBe Map("y" -> "3")
  }

  it should "take a property away with unset, and let a later section set it again" in {
    val file = layer("/project", "[*]\nindent_size = 2\n[*.md]\nindent_size = unset\n[a.md]\nindent_style = tab\n")

    properties("/project/a.md", file) shouldBe Map("indent_style" -> "tab")
    properties("/project/a.txt", file) shouldBe Map("indent_size" -> "2")
  }

  it should "let a nearer file unset what a farther one set" in {
    val farther = layer("/project", "[*]\nindent_size = 2\n")
    val nearer  = layer("/project/sub", "[*]\nindent_size = unset\n")

    properties("/project/sub/a.txt", farther, nearer) shouldBe Map.empty
  }

  "The indentation keys" should "give spaces and their size" in {
    EditorConfig.indentation(Map("indent_style" -> "space", "indent_size" -> "2")) shouldBe
      IndentationHint(insertSpaces = Some(true), tabSize = Some(2))
  }

  it should "give tabs the tab width, and the indent size when there is none" in {
    EditorConfig.indentation(Map("indent_style" -> "tab", "indent_size" -> "2", "tab_width" -> "8")) shouldBe
      IndentationHint(insertSpaces = Some(false), tabSize = Some(8))
    EditorConfig.indentation(Map("indent_style" -> "tab", "indent_size" -> "2")) shouldBe
      IndentationHint(insertSpaces = Some(false), tabSize = Some(2))
  }

  it should "read indent_size = tab as the tab width" in {
    EditorConfig.indentation(Map("indent_style" -> "space", "indent_size" -> "tab", "tab_width" -> "3")) shouldBe
      IndentationHint(insertSpaces = Some(true), tabSize = Some(3))
    EditorConfig.indentation(Map("indent_style" -> "tab", "tab_width" -> "6")) shouldBe
      IndentationHint(insertSpaces = Some(false), tabSize = Some(6))
  }

  it should "leave a field unsaid when its key is missing or invalid" in {
    EditorConfig.indentation(Map("indent_style" -> "tab")) shouldBe IndentationHint(insertSpaces = Some(false))
    EditorConfig.indentation(Map("indent_style" -> "both", "indent_size" -> "0")) shouldBe IndentationHint.Empty
    EditorConfig.indentation(Map("tab_width" -> "5")) shouldBe IndentationHint(tabSize = Some(5))
    EditorConfig.indentation(Map.empty) shouldBe IndentationHint.Empty
  }

end EditorConfigSpec
