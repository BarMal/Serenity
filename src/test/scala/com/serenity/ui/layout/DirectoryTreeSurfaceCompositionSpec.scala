package com.serenity.ui.layout

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `DirectoryTreeSurfaceComposition` (issue #819, slice 4): the pinned/expanded directory tree panel
  * resolved into one paint/hit-test plan, reusing `PanelContentResolver.directoryTreeRowViews`'s row text and clipping
  * so painting and mouse hit-testing can never disagree about which path a row represents.
  */
class DirectoryTreeSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val root = Paths.get("/repo")
  private val src  = root.resolve("src")
  private val test = root.resolve("test")

  private val tree = DirectoryTreeData(
    rootPath = root,
    expandedPaths = Set(root),
    entries = Map(
      root -> List(
        DirEntry(src, "src", isDirectory = true),
        DirEntry(test, "test", isDirectory = true),
        DirEntry(root.resolve("build.sbt"), "build.sbt", isDirectory = false)
      )
    )
  )

  "forTree" should "paint one text box per visible row, matching PanelContentResolver's row text" in {
    val frameRect    = LayoutRect(0, 0, 24, 10)
    val expectedRows = PanelContentResolver.directoryTreeRowViews(frameRect, tree, selectedPath = None)

    val resolved = DirectoryTreeSurfaceComposition.forTree(tree, selectedPath = None, frameRect)

    resolved.paintBoxes.map(_.text) shouldBe expectedRows.map(view => Some(view.row.plainText))
  }

  it should "mark the selected path's row as selected" in {
    val frameRect = LayoutRect(0, 0, 24, 10)

    val resolved = DirectoryTreeSurfaceComposition.forTree(tree, selectedPath = Some(src), frameRect)

    resolved.paintBoxes.map(_.selected) shouldBe List(false, true, false, false)
  }

  it should "resolve hitAt to the exact row clicked, addressed by its filesystem path" in {
    val frameRect = LayoutRect(0, 0, 24, 10)

    val resolved = DirectoryTreeSurfaceComposition.forTree(tree, selectedPath = None, frameRect)

    val testBox = resolved.paintBoxes.find(_.text.exists(_.endsWith("test"))).getOrElse(fail("expected a row for test"))
    val hit     = resolved.hitAt(testBox.rect.x, testBox.rect.y)

    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId(test.toString))
  }

  it should "clip rows to the panel's height the same way PanelContentResolver does" in {
    val frameRect    = LayoutRect(0, 0, 24, 4)
    val expectedRows = PanelContentResolver.directoryTreeRowViews(frameRect, tree, selectedPath = None)

    val resolved = DirectoryTreeSurfaceComposition.forTree(tree, selectedPath = None, frameRect)

    resolved.paintBoxes.size shouldBe expectedRows.size
    resolved.paintBoxes.size should be < DirectoryTreeData.visibleRows(tree).size
  }

  it should "expose a focus order matching on-screen row order" in {
    val frameRect = LayoutRect(0, 0, 24, 10)

    val resolved = DirectoryTreeSurfaceComposition.forTree(tree, selectedPath = None, frameRect)

    resolved.focusOrder.size shouldBe resolved.paintBoxes.size
  }
end DirectoryTreeSurfaceCompositionSpec
