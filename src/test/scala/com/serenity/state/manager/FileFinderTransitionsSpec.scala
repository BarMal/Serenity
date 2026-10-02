package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.command.FileFinderCommands
import com.serenity.io.ProjectFileListing
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer}
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileFinderTransitionsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val root  = Path.of("/work/project")
  private val files = ProjectFileListing(Vector("docs/guide.md", "src/main/App.scala").map(Path.of(_)), false)

  private def opened: (AppState, SurfaceId) =
    FileFinderTransitions.withFinderOpened(AppState.initial, root).getOrElse(fail("no finder"))

  private def finder(state: AppState, id: SurfaceId): Option[ListPicker] =
    state.runtime.uiSurfaces.find(_.id == id).collect {
      case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) => picker
    }

  private def typed(state: AppState, text: String): AppState =
    text.foldLeft(state)((current, char) =>
      ModalEventReducer.reduce(ModalType.ListPicker, ModalInsertChar(char), current).state
    )

  private def labels(state: AppState, id: SurfaceId): Vector[String] =
    finder(state, id).flatMap(_.items.toOption).fold(Vector.empty)(_.items.map(_.label))

  "Opening the file finder" should "show it at once, still loading, with a query field to type into" in {
    val (state, id) = opened

    finder(state, id).map(picker => (picker.title, picker.items, picker.query.isDefined)) shouldBe
      Some(("Go to File", Loadable.Loading(), true))
    state.modalSurface.map(_.id) shouldBe Some(id)
  }

  "A project file listing" should "fill the finder it was taken for" in {
    val (state, id) = opened

    labels(FileFinderTransitions.withFilesListed(state, id, root, Right(files)), id) shouldBe
      Vector("guide.md", "App.scala")
  }

  it should "rank the listing by what was typed while it was loading" in {
    val (state, id) = opened
    val early       = typed(state, "app")

    labels(FileFinderTransitions.withFilesListed(early, id, root, Right(files)), id) shouldBe Vector("App.scala")
  }

  it should "show why the files couldn't be listed" in {
    val (state, id) = opened

    finder(FileFinderTransitions.withFilesListed(state, id, root, Left("denied")), id).map(_.items) shouldBe
      Some(Loadable.Failed("denied"))
  }

  it should "be dropped once the finder has closed" in {
    val (state, id) = opened
    val dismissed   = WorkflowSurfaces.dismissedToPriorFocus(state, id)

    FileFinderTransitions.withFilesListed(dismissed, id, root, Right(files)) shouldBe dismissed
  }

  it should "be dropped when it lists another root than the finder's" in {
    val (state, id) = opened

    FileFinderTransitions.withFilesListed(state, id, Path.of("/elsewhere"), Right(files)) shouldBe state
  }

  "Typing in a filled finder" should "re-rank its files, and Enter opens the highlighted one" in {
    val (state, id) = opened
    val filled      = FileFinderTransitions.withFilesListed(state, id, root, Right(files))
    val narrowed    = typed(filled, "gd")

    labels(narrowed, id) shouldBe Vector("guide.md")
    val picked = ModalEventReducer.reduce(ModalType.ListPicker, ModalSubmit, narrowed)
    picked.effects shouldBe List(AppEffect.ExecuteCommand(FileFinderCommands.openFile(root.resolve("docs/guide.md"))))
    picked.state.modalSurface shouldBe None
  }

  "An effect result for a listing" should "apply through the dispatcher's result path" in {
    val (state, id) = opened

    labels(EffectResult.applyIfCurrent(state, EffectResult.FilesListed(id, root, Right(files))), id) shouldBe
      Vector("guide.md", "App.scala")
  }
