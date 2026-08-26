package com.serenity.io

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileUtilsSpec extends AnyFlatSpec with Matchers:

  private val userHome = Paths.get(System.getProperty("user.home"))

  "resolvePath" should "expand a bare tilde to the user's home directory" in {
    FileUtils.resolvePath("~").unsafeRunSync() shouldBe userHome.normalize()
  }

  it should "expand a tilde-prefixed relative path against the home directory" in {
    FileUtils.resolvePath("~/notes/todo.md").unsafeRunSync() shouldBe userHome.resolve("notes/todo.md").normalize()
  }

  it should "leave an absolute path unaffected" in {
    FileUtils.resolvePath("/etc/hosts").unsafeRunSync() shouldBe Paths.get("/etc/hosts")
  }

  it should "resolve a plain relative path against the current directory" in {
    val currentDir = FileUtils.getCurrentDirectory.unsafeRunSync()
    FileUtils.resolvePath("notes/todo.md").unsafeRunSync() shouldBe currentDir.resolve("notes/todo.md").normalize()
  }

  it should "not treat a tilde inside a filename (not at the start) as home expansion" in {
    val currentDir = FileUtils.getCurrentDirectory.unsafeRunSync()
    FileUtils.resolvePath("notes/~backup.md").unsafeRunSync() shouldBe
      currentDir.resolve("notes/~backup.md").normalize()
  }
