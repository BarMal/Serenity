package com.serenity

import java.nio.file.Paths

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverDirectorySpec extends AnyFlatSpec with Matchers:

  private val root = Paths.get("/repo")

  private val entries = List(
    DirEntry(root.resolve("src"), "src", isDirectory = true),
    DirEntry(root.resolve("test"), "test", isDirectory = true),
    DirEntry(root.resolve("build.sbt"), "build.sbt", isDirectory = false)
  )

  private val tree = DirectoryTreeData(
    rootPath = root,
    expandedPaths = Set(root, root.resolve("src")),
    entries = Map(
      root -> entries,
      root.resolve("src") -> List(
        DirEntry(root.resolve("src").resolve("main"), "main", isDirectory = true),
        DirEntry(root.resolve("src").resolve("Serenity.scala"), "Serenity.scala", isDirectory = false)
      )
    )
  )

  "SurfaceContentResolver" should "shape the same directory content differently when floating versus pinned" in {
    val content = SurfaceContent.DirectoryListing(root, entries, Some(root.resolve("src")))

    val floating = SurfaceContentResolver.resolve(
      content,
      LayoutRect(0, 0, 24, 20),
      SurfaceRenderMode.Floating
    )
    val pinned = SurfaceContentResolver.resolve(
      content,
      LayoutRect(0, 0, 24, 20),
      SurfaceRenderMode.Pinned
    )

    floating.title shouldBe None
    floating.rows.map(_.plainText) shouldBe List("Directory: repo", "src", "test", "build.sbt")

    pinned.title shouldBe Some("repo")
    pinned.rows.map(_.plainText) shouldBe List("Selected: src", "src", "test", "build.sbt")
  }

  it should "resolve directory trees with an explicit root row and lazy-load markers" in {
    val content = SurfaceContent.DirectoryTree(tree, Some(root.resolve("src")))

    val pinned = SurfaceContentResolver.resolve(
      content,
      LayoutRect(0, 0, 24, 20),
      SurfaceRenderMode.Pinned
    )

    pinned.title shouldBe Some("repo")
    pinned.rows.map(_.plainText) shouldBe List(
      "▾ repo",
      "  ▾ src",
      "    ▹ main",
      "    Serenity.scala",
      "  ▹ test",
      "  build.sbt"
    )
    pinned.rows.count(_.selected) shouldBe 1
    pinned.rows.find(_.selected).map(_.plainText) shouldBe Some("  ▾ src")
  }

end SurfaceContentResolverDirectorySpec
