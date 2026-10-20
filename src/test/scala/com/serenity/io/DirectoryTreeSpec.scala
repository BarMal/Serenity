package com.serenity.io

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DirectoryTreeSpec extends AnyFlatSpec with Matchers:

  private def tree(parent: Path): Path =
    val root = Files.createDirectories(parent.resolve("root").resolve("a").resolve("b")).getParent.getParent
    val _    = Files.writeString(root.resolve("a").resolve("b").resolve("deep.txt"), "x")
    val _    = Files.writeString(root.resolve("top.txt"), "x")
    root

  "DirectoryTree.deleteRecursively" should "remove a folder with nested files" in
    TestTemp.within("directory-tree") { parent =>
      val root = tree(parent)

      DirectoryTree.deleteRecursively(root).unsafeRunSync()

      Files.exists(root) shouldBe false
    }

  it should "do nothing for a path that is not there" in
    TestTemp.within("directory-tree-absent") { parent =>
      noException should be thrownBy DirectoryTree.deleteRecursively(parent.resolve("absent")).unsafeRunSync()
    }

  it should "remove a link without touching what it points at" in
    TestTemp.within("directory-tree-link") { parent =>
      val outside = Files.createDirectories(parent.resolve("outside"))
      val kept    = Files.writeString(outside.resolve("kept.txt"), "x")
      val root    = Files.createDirectories(parent.resolve("root"))
      val _       = Files.createSymbolicLink(root.resolve("link"), outside)

      DirectoryTree.deleteRecursively(root).unsafeRunSync()

      Files.exists(root) shouldBe false
      Files.exists(kept) shouldBe true
    }
