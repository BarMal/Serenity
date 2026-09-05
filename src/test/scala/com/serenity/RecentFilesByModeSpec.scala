package com.serenity

import java.nio.file.Paths

import com.serenity.config.AppMode
import com.serenity.state.models.Persisted
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Recent files tagged with the app mode active when they were opened (issue #1307), so the mode/tab widget can offer
  * "recent projects/sessions opened in this mode" rather than one undifferentiated list.
  */
class RecentFilesByModeSpec extends AnyFlatSpec with Matchers:

  private val codeFile  = Paths.get("/tmp/main.scala")
  private val proseFile = Paths.get("/tmp/notes.md")
  private val codeFile2 = Paths.get("/tmp/other.scala")

  "Persisted.trackRecentFile" should "record a path under the mode it was opened in" in {
    val updated = Persisted.trackRecentFile(Map.empty, AppMode.Code, codeFile)

    updated shouldBe Map(AppMode.Code -> List(codeFile))
  }

  it should "keep each mode's list independent" in {
    val afterCode  = Persisted.trackRecentFile(Map.empty, AppMode.Code, codeFile)
    val afterProse = Persisted.trackRecentFile(afterCode, AppMode.Prose, proseFile)

    afterProse shouldBe Map(AppMode.Code -> List(codeFile), AppMode.Prose -> List(proseFile))
  }

  it should "move a re-opened path to the front of its mode's list rather than duplicating it" in {
    val first  = Persisted.trackRecentFile(Map.empty, AppMode.Code, codeFile)
    val second = Persisted.trackRecentFile(first, AppMode.Code, codeFile2)
    val third  = Persisted.trackRecentFile(second, AppMode.Code, codeFile)

    third shouldBe Map(AppMode.Code -> List(codeFile, codeFile2))
  }

  it should "cap each mode's list at 20 entries" in {
    val paths = (1 to 25).map(i => Paths.get(s"/tmp/file$i.scala")).toList
    val result = paths.foldLeft(Map.empty[AppMode, List[java.nio.file.Path]]) { (acc, path) =>
      Persisted.trackRecentFile(acc, AppMode.Code, path)
    }

    result(AppMode.Code) should have size 20
    result(AppMode.Code).head shouldBe paths.last
  }
