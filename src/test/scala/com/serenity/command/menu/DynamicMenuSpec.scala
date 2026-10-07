package com.serenity.command.menu

import java.nio.file.{Path, Paths}

import com.serenity.TestWorkspaceTrees
import com.serenity.command.CommandRegistry
import com.serenity.command.menu.MenuDispatch.Choice
import com.serenity.keystroke.events.{ActivateBuffer, OpenRecentPath}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DynamicMenuSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI

  private def withRecent(paths: List[Path]): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(recentFiles = paths))

  private def buffer(id: Int, file: Option[String], dirty: Boolean = false): Buffer =
    val base = Buffer.fromString(BufferId(id), "text")
    base.copy(document = base.document.copy(filePath = file.map(Paths.get(_)), isDirty = dirty))

  private def absolute(path: String): Path = Paths.get(path).toAbsolutePath.normalize

  private def withBuffers(buffers: List[Buffer], active: Int): AppState =
    val pane = EditorPane.withBuffer(PaneId(0), BufferId(active))
    AppState(persisted =
      Persisted(
        layout = Layout(
          editorPanes = Map(PaneId(0) -> pane),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        buffers = buffers.map(b => b.id -> b).toMap,
        focus = Focus.EditorPane(PaneId(0)),
        bufferOrder = buffers.map(_.id)
      )
    )

  private def recent(app: AppState): DynamicMenu.Section  = DynamicMenu.expand(DynamicSource.RecentFiles, app, registry)
  private def windows(app: AppState): DynamicMenu.Section = DynamicMenu.expand(DynamicSource.OpenBuffers, app, registry)

  private def chosen(section: DynamicMenu.Section): List[DynamicMenu.Item.Choose] =
    section.items.collect { case choose: DynamicMenu.Item.Choose => choose }

  "Open Recent" should "be a submenu titled for what it holds" in {
    recent(withRecent(List(Paths.get("/a/one.md")))).submenuTitle shouldBe Some("Open Recent")
  }

  it should "list the most recent file first, labelled by file name with the directory as its description" in {
    val section = recent(withRecent(List(Paths.get("/work/notes/new.md"), Paths.get("/home/me/old.txt"))))

    chosen(section).map(c => (c.label, c.description)) shouldBe
      List(("new.md", Some(absolute("/work/notes").toString)), ("old.txt", Some(absolute("/home/me").toString)))
    chosen(section).map(_.choice) shouldBe
      List(Choice.RecentFile(Paths.get("/work/notes/new.md")), Choice.RecentFile(Paths.get("/home/me/old.txt")))
  }

  it should "show a file once, at its most recent place" in {
    val section = recent(withRecent(List(Paths.get("/a/x.md"), Paths.get("/b/y.md"), Paths.get("/a/./x.md"))))

    chosen(section).map(_.label) shouldBe List("x.md", "y.md")
  }

  it should "show no more than the start page's recent list does" in {
    val many    = (1 to 20).map(i => Paths.get(s"/dir/file$i.md")).toList
    val section = recent(withRecent(many))

    DynamicMenu.RecentFilesLimit shouldBe StartupPageContent.RecentFilesLimit
    chosen(section).map(_.label) shouldBe many.take(StartupPageContent.RecentFilesLimit).map(_.getFileName.toString)
  }

  it should "end with a Clear Recent Files command after a separator" in {
    val section = recent(withRecent(List(Paths.get("/a/one.md"))))
    val clear   = registry.findCommand("clear-recent-files").getOrElse(fail("clear-recent-files is not registered"))

    section.items.takeRight(2) shouldBe List(DynamicMenu.Item.Separator, DynamicMenu.Item.Run(clear))
  }

  it should "show only a disabled placeholder when nothing was opened recently" in {
    recent(withRecent(Nil)).items shouldBe List(DynamicMenu.Item.Placeholder("(No recent files)"))
  }

  it should "give files no mnemonic and never check one" in {
    val files = chosen(recent(withRecent(List(Paths.get("/a/one.md")))))

    files.map(_.mnemonic) shouldBe List(None)
    files.map(_.checked) shouldBe List(false)
  }

  "Window" should "list the open buffers in tab order, inline" in {
    val app     = withBuffers(List(buffer(0, Some("/p/b.md")), buffer(1, Some("/p/a.md")), buffer(2, None)), active = 0)
    val section = windows(app)

    section.submenuTitle shouldBe None
    chosen(section).map(_.choice) shouldBe
      List(Choice.Buffer(BufferId(0)), Choice.Buffer(BufferId(1)), Choice.Buffer(BufferId(2)))
    chosen(section).map(_.label.drop(2)) shouldBe List("b.md", "a.md", "Buffer 2")
  }

  it should "check only the active buffer" in {
    val app = withBuffers(List(buffer(0, Some("/p/a.md")), buffer(1, Some("/p/b.md"))), active = 1)

    chosen(windows(app)).map(_.checked) shouldBe List(false, true)
  }

  it should "mark a dirty buffer with the glyph the tab bar uses" in {
    val app = withBuffers(List(buffer(0, Some("/p/a.md")), buffer(1, Some("/p/b.md"), dirty = true)), active = 0)

    chosen(windows(app)).map(_.label) shouldBe List("1 a.md", "2 b.md ●")
    TabListContent.build(app).entries.map(_.isDirty) shouldBe List(false, true)
  }

  it should "describe a buffer by its path, and say when it has unsaved changes" in {
    val app =
      withBuffers(List(buffer(0, Some("/p/a.md")), buffer(1, Some("/p/b.md"), dirty = true), buffer(2, None)), 0)

    chosen(windows(app)).map(_.description) shouldBe
      List(Some(Paths.get("/p/a.md").toString), Some(s"${Paths.get("/p/b.md")} (unsaved changes)"), None)
  }

  it should "number the first nine buffers as mnemonics 1 to 9 and leave the rest without" in {
    val buffers = (0 until 12).map(i => buffer(i, Some(s"/p/f$i.md"))).toList
    val section = chosen(windows(withBuffers(buffers, active = 0)))

    section.take(9).map(_.mnemonic) shouldBe (1 to 9).map(n => Some(MenuMnemonics.Mnemonic(0, ('0' + n).toChar)))
    section.take(9).map(_.label.take(2)) shouldBe (1 to 9).map(n => s"$n ")
    section.drop(9).map(_.mnemonic) shouldBe List(None, None, None)
    section.drop(9).map(_.label) shouldBe List("f9.md", "f10.md", "f11.md")
  }

  it should "list every buffer, however many are open" in {
    val buffers = (0 until 40).map(i => buffer(i, Some(s"/p/f$i.md"))).toList

    chosen(windows(withBuffers(buffers, active = 0))) should have size 40
  }

  it should "have no items when no buffer is open" in {
    windows(withBuffers(Nil, active = 0)).items shouldBe Nil
  }

  "A chosen dynamic item" should "send exactly one event: the buffer to activate or the path to open" in {
    val app     = withBuffers(List(buffer(0, Some("/p/a.md")), buffer(1, Some("/p/b.md"))), active = 0)
    val oneFile = recent(withRecent(List(Paths.get("/a/one.md"))))

    chosen(windows(app)).map(c => List(MenuDispatch.eventFor(c.choice))) shouldBe
      List(List(ActivateBuffer(BufferId(0))), List(ActivateBuffer(BufferId(1))))
    chosen(oneFile).map(c => List(MenuDispatch.eventFor(c.choice))) shouldBe
      List(List(OpenRecentPath(absolute("/a/one.md"))))
  }
