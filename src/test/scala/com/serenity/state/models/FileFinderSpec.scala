package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.FileFinderCommands
import com.serenity.io.ProjectFileListing
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.widget.{Loadable, TextField}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileFinderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val root = Path.of("/work/project")

  private def listing(paths: String*): ProjectFileListing =
    ProjectFileListing(paths.toVector.map(Path.of(_)), truncated = false)

  private def listed(files: ProjectFileListing, query: String = "", state: AppState = AppState.initial): ListPicker =
    val picker = FileFinder.picker(root)
    ListPickerSearch.refreshed(
      picker.copy(
        query = Some(TextField.of(query)),
        source = Some(PickerSource.ProjectFiles(root, Loadable.Ready(files)))
      ),
      state
    )

  private def shown(picker: ListPicker): Vector[(String, Option[String])] =
    picker.items.toOption.fold(Vector.empty)(_.items.map(choice => (choice.label, choice.detail)))

  "The file finder" should "open titled, with an empty query and its files still loading" in {
    val picker = FileFinder.picker(root)

    picker.title shouldBe "Go to File"
    picker.items shouldBe Loadable.Loading()
    picker.queryText shouldBe ""
    picker.source shouldBe Some(PickerSource.ProjectFiles(root, Loadable.Loading()))
  }

  it should "stay loading while the query changes before the listing lands" in {
    val typed = FileFinder.picker(root).copy(query = Some(TextField.of("app")))

    ListPickerSearch.refreshed(typed, AppState.initial).items shouldBe Loadable.Loading()
  }

  it should "show why the listing failed" in {
    val failed = FileFinder.picker(root).copy(source = Some(PickerSource.ProjectFiles(root, Loadable.Failed("denied"))))

    ListPickerSearch.refreshed(failed, AppState.initial).items shouldBe Loadable.Failed("denied")
  }

  it should "show each file by name, with its directory relative to the root as detail" in {
    shown(listed(listing("README.md", "src/main/App.scala"))) shouldBe Vector(
      ("README.md", None),
      ("App.scala", Some("src/main"))
    )
  }

  it should "open the picked file by its full path" in {
    listed(listing("src/main/App.scala")).selectedChoice.map(_.action) shouldBe
      Some(FileFinderCommands.openFile(root.resolve("src/main/App.scala")))
  }

  it should "rank files by how well they match the query, dropping those that don't match" in {
    val files = listing("app/build.txt", "docs/notes.md", "src/Application.scala", "src/main/app.scala")

    shown(listed(files, "app")).map(_._1) shouldBe Vector("app.scala", "Application.scala", "build.txt")
  }

  it should "say so when nothing matches, or when there are no files at all" in {
    listed(listing("a.txt"), "zzz").items shouldBe Loadable.Empty(ListPicker.NoMatches)
    listed(listing()).items shouldBe Loadable.Empty("No files under /work/project")
  }

  it should "list recently opened files under the root first for an empty query, then the rest by path" in {
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(recentFiles = List(root.resolve("src/z.txt"), Path.of("/elsewhere/other.txt"), root.resolve("b.txt")))
    )

    shown(listed(listing("a.txt", "b.txt", "src/y.txt", "src/z.txt"), state = state)).map(_._1) shouldBe
      Vector("z.txt", "b.txt", "a.txt", "y.txt")
  }

  it should "show at most a bounded number of rows" in {
    val many = listing((1 to FileFinder.MaxShownChoices + 50).map(index => f"file-$index%04d.txt")*)

    listed(many).items.toOption.map(_.items.size) shouldBe Some(FileFinder.MaxShownChoices)
    listed(many, "file").items.toOption.map(_.items.size) shouldBe Some(FileFinder.MaxShownChoices)
  }

  it should "say in its title when the listing was cut short" in {
    val cut = ProjectFileListing(Vector(Path.of("a.txt")), truncated = true)

    listed(cut).title shouldBe "Go to File (first 20,000 files)"
    listed(listing("a.txt")).title shouldBe "Go to File"
  }

  "The project root" should "be the docked explorer's root when one is pinned" in {
    val pinned = com.serenity.state.reducers.PinnedPanelContentReducer
      .pinExplorerRoot(PanelPosition.Left, root, 30, AppState.initial)
      .state

    FileFinder.explorerRoot(pinned) shouldBe Some(root)
    FileFinder.explorerRoot(AppState.initial) shouldBe None
  }
