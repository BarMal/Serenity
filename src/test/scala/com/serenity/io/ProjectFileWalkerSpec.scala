package com.serenity.io

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectFileWalkerSpec extends AnyFlatSpec with Matchers:

  private def tree(paths: String*): Path =
    val root = Files.createTempDirectory("project-file-walker")
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

    ProjectFileWalker.list(root, limit = 100).unsafeRunSync().files.map(_.toString) shouldBe
      Vector(".scalafmt.conf", "kept.txt", "src/targets.txt")
  }

  it should "stop at the limit, keeping the shallowest files, and say the listing was cut short" in {
    val root    = tree("top-1.txt", "top-2.txt", "deep/a/b/c.txt", "deep/a/d.txt")
    val listing = ProjectFileWalker.list(root, limit = 3).unsafeRunSync()

    listing.truncated shouldBe true
    listing.files.map(_.toString) shouldBe Vector("deep/a/d.txt", "top-1.txt", "top-2.txt")
  }

  it should "not call a listing that exactly fills the limit truncated" in {
    val root = tree("one.txt", "two.txt")

    ProjectFileWalker.list(root, limit = 2).unsafeRunSync().truncated shouldBe false
  }

  it should "fail when the root cannot be listed" in {
    val missing = Files.createTempDirectory("project-file-walker").resolve("missing")

    ProjectFileWalker.list(missing, limit = 10).attempt.unsafeRunSync().isLeft shouldBe true
  }
