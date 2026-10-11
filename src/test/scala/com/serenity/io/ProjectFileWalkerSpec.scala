package com.serenity.io

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectFileWalkerSpec extends AnyFlatSpec with Matchers:

  private def tree(paths: String*): Path =
    val root = TestTemp.directory("project-file-walker")
    paths.foreach { relative =>
      val file = root.resolve(relative)
      Files.createDirectories(file.getParent)
      Files.writeString(file, relative)
    }
    root

  "ProjectFileWalker" should "list every file under the root, relative to it and sorted by path" in {
    val root = tree("b.txt", "src/main/App.scala", "a.txt", "src/Lib.scala")

    ProjectFileWalker.list(root, limit = 100).unsafeRunSync() shouldBe ProjectFileListing(
      Vector("a.txt", "b.txt", "src/Lib.scala", "src/main/App.scala").map(Path.of(_)),
      truncated = false
    )
  }

  it should "skip hidden directories and build or dependency output, but keep hidden files" in {
    val root = tree(
      "kept.txt",
      ".scalafmt.conf",
      ".git/HEAD",
      ".metals/metals.log",
      ".idea/workspace.xml",
      "target/classes/App.class",
      "node_modules/left-pad/index.js",
      "dist/bundle.js",
      "build/out.o",
      "src/target/nested.class",
      "src/targets.txt"
    )

    ProjectFileWalker.list(root, limit = 100).unsafeRunSync().files shouldBe
      Vector(".scalafmt.conf", "kept.txt", "src/targets.txt").map(Path.of(_))
  }

  it should "stop at the limit, keeping the shallowest files, and say the listing was cut short" in {
    val root    = tree("top-1.txt", "top-2.txt", "deep/a/b/c.txt", "deep/a/d.txt")
    val listing = ProjectFileWalker.list(root, limit = 3).unsafeRunSync()

    listing.truncated shouldBe true
    listing.files shouldBe Vector("deep/a/d.txt", "top-1.txt", "top-2.txt").map(Path.of(_))
  }

  it should "not call a listing that exactly fills the limit truncated" in {
    val root = tree("one.txt", "two.txt")

    ProjectFileWalker.list(root, limit = 2).unsafeRunSync().truncated shouldBe false
  }

  it should "fail when the root cannot be listed" in {
    val missing = TestTemp.directory("project-file-walker").resolve("missing")

    ProjectFileWalker.list(missing, limit = 10).attempt.unsafeRunSync().isLeft shouldBe true
  }

  private def write(root: Path, relative: String, content: String): Unit =
    val file = root.resolve(relative)
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)

  private def listed(root: Path): Vector[String] =
    ProjectFileWalker.list(root, limit = 100000).unsafeRunSync().files.map(_.toString)

  it should "leave out files and directories the root .gitignore matches" in {
    val root = tree("kept.txt", "debug.log", "out/App.txt", "src/trace.log", "src/Main.scala")
    write(root, ".gitignore", "*.log\nout/\n")

    listed(root) shouldBe Vector(".gitignore", "kept.txt", "src/Main.scala")
  }

  it should "scope a nested .gitignore to its own directory and let it override its parents" in {
    val root = tree("local.txt", "pkg/local.txt", "pkg/inner/local.txt", "pkg/keep.log", "top.log")
    write(root, ".gitignore", "*.log\n")
    write(root, "pkg/.gitignore", "local.txt\n!keep.log\n")

    listed(root) shouldBe Vector(".gitignore", "local.txt", "pkg/.gitignore", "pkg/keep.log")
  }

  it should "honour negation, but never re-include a file inside an ignored directory" in {
    val root = tree("a.log", "important.log", "cache/important.log", "cache/other.txt")
    write(root, ".gitignore", "*.log\n!important.log\ncache/\n!cache/important.log\n")

    listed(root) shouldBe Vector(".gitignore", "important.log")
  }

  it should "anchor a pattern with a leading slash to the directory holding the .gitignore" in {
    val root = tree("notes.txt", "docs/notes.txt", "docs/deep/notes.txt")
    write(root, ".gitignore", "/notes.txt\n")

    listed(root) shouldBe Vector(".gitignore", "docs/deep/notes.txt", "docs/notes.txt")
  }

  it should "honour .ignore files and the repository's info/exclude" in {
    val root = tree("a.tmp", "b.bak", "c.txt")
    write(root, ".ignore", "*.tmp\n")
    write(root, ".git/info/exclude", "*.bak\n")

    listed(root) shouldBe Vector(".ignore", "c.txt")
  }

  it should "leave out compiled binaries by default" in {
    val root = tree("Main.scala", "lib/Main.class", "lib/dep.jar")

    listed(root) shouldBe Vector("Main.scala")
  }
