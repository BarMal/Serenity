package com.serenity.app

import java.nio.file.Path

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A folder on the command line is the project root, as Open Folder makes it; only one folder can be. */
class LaunchOpensSpec extends AnyFlatSpec with Matchers:

  private def path(name: String): Path = Path.of("/work", name)
  private val folders                  = Set("src", "docs", "lib").map(path)
  private def planned(names: String*)  = LaunchOpens.plan(names.toList.map(path), folders.contains)

  "LaunchOpens.plan" should "leave a list of files as it was, with no root" in {
    planned("a.md", "b.md") shouldBe LaunchOpens(None, List(path("a.md"), path("b.md")), Nil)
  }

  it should "take a lone folder as the root" in {
    planned("src") shouldBe LaunchOpens(Some(path("src")), Nil, Nil)
  }

  it should "take a folder as the root and still open the files around it, in order" in {
    planned("a.md", "src", "b.md") shouldBe LaunchOpens(Some(path("src")), List(path("a.md"), path("b.md")), Nil)
  }

  it should "take the first of several folders as the root and set the others aside" in {
    planned("docs", "a.md", "src", "lib") shouldBe
      LaunchOpens(Some(path("docs")), List(path("a.md")), List(path("src"), path("lib")))
  }

  "LaunchOpens.notice" should "say nothing when no folder was set aside" in {
    planned("src", "a.md").notice shouldBe None
  }

  it should "name the root kept and the folders set aside" in {
    planned("docs", "src", "lib").notice shouldBe Some(
      s"Only one folder can be the project root. Opened ${path("docs")}; did not open ${path("src")}, ${path("lib")}."
    )
  }

  "LaunchOpens.resolve" should "ask the filesystem which paths are folders" in {
    import cats.effect.unsafe.implicits.global
    val dir  = java.nio.file.Files.createTempDirectory("launch-opens")
    val file = java.nio.file.Files.createFile(dir.resolve("a.md"))
    try LaunchOpens.resolve(List(file, dir)).unsafeRunSync() shouldBe LaunchOpens(Some(dir), List(file), Nil)
    finally
      java.nio.file.Files.deleteIfExists(file)
      java.nio.file.Files.deleteIfExists(dir)
  }
