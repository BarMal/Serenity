package com.serenity.io

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.state.models.IndentationHint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Finding the `.editorconfig` files above a path on disk (#1996). */
class EditorConfigResolverSpec extends AnyFlatSpec with Matchers:

  private def write(path: Path, text: String): Path =
    val _ = Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def indentation(resolver: EditorConfigResolver, file: Path): IndentationHint =
    resolver.indentationFor(file).unsafeRunSync()

  private val twoSpaces = IndentationHint(insertSpaces = Some(true), tabSize = Some(2))

  "The resolver" should "read the file in the directory of the path" in {
    val root = TestTemp.directory("editorconfig-same")
    val _    = write(root.resolve(".editorconfig"), "root = true\n[*]\nindent_style = space\nindent_size = 2\n")
    val file = write(root.resolve("a.txt"), "x")

    indentation(EditorConfigResolver(), file) shouldBe twoSpaces
  }

  it should "climb to files above and let the nearer one win" in {
    val root = TestTemp.directory("editorconfig-climb")
    val _    = write(root.resolve(".editorconfig"), "root = true\n[*]\nindent_style = space\nindent_size = 2\n")
    val _    = write(root.resolve("sub/.editorconfig"), "[*.txt]\nindent_size = 8\n")
    val deep = write(root.resolve("sub/deeper/a.txt"), "x")
    val top  = write(root.resolve("b.txt"), "x")

    indentation(EditorConfigResolver(), deep) shouldBe IndentationHint(Some(true), Some(8))
    indentation(EditorConfigResolver(), top) shouldBe twoSpaces
  }

  it should "stop climbing at a file that says root = true" in {
    val root = TestTemp.directory("editorconfig-root")
    val _    = write(root.resolve(".editorconfig"), "[*]\nindent_style = tab\n")
    val _    = write(root.resolve("sub/.editorconfig"), "root = true\n[*]\nindent_size = 2\n")
    val file = write(root.resolve("sub/a.txt"), "x")

    indentation(EditorConfigResolver(), file) shouldBe IndentationHint(tabSize = Some(2))
  }

  it should "read an edited file again on the next lookup" in {
    val root     = TestTemp.directory("editorconfig-edit")
    val config   = write(root.resolve(".editorconfig"), "root = true\n[*]\nindent_size = 2\n")
    val file     = write(root.resolve("a.txt"), "x")
    val resolver = EditorConfigResolver()

    indentation(resolver, file).tabSize shouldBe Some(2)

    val _ = Files.writeString(config, "root = true\n[*]\nindent_size = 6\n")

    indentation(resolver, file).tabSize shouldBe Some(6)
  }

  it should "read a file again that was rewritten to the same size within one timestamp tick (#2348)" in {
    val root     = TestTemp.directory("editorconfig-same-stamp")
    val config   = write(root.resolve(".editorconfig"), "root = true\n[*]\nindent_size = 2\n")
    val file     = write(root.resolve("a.txt"), "x")
    val resolver = EditorConfigResolver()
    val stamped  = Files.getLastModifiedTime(config)

    indentation(resolver, file).tabSize shouldBe Some(2)

    // Same length, and the timestamp put back: exactly what a coarse filesystem clock leaves after a fast re-save.
    val _ = Files.writeString(config, "root = true\n[*]\nindent_size = 6\n")
    val _ = Files.setLastModifiedTime(config, stamped)

    Files.size(config) shouldBe "root = true\n[*]\nindent_size = 2\n".length.toLong
    Files.getLastModifiedTime(config) shouldBe stamped
    indentation(resolver, file).tabSize shouldBe Some(6)
  }

  it should "forget a file that was deleted" in {
    val root     = TestTemp.directory("editorconfig-delete")
    val config   = write(root.resolve(".editorconfig"), "root = true\n[*]\nindent_size = 2\n")
    val file     = write(root.resolve("a.txt"), "x")
    val resolver = EditorConfigResolver()

    indentation(resolver, file).tabSize shouldBe Some(2)

    Files.delete(config)

    indentation(resolver, file) shouldBe IndentationHint.Empty
  }

end EditorConfigResolverSpec
