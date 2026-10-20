package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.frontend.FrontendCapabilities
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartPageTransitionsSpec extends AnyFlatSpec with Matchers:

  private def returnedTo(capabilities: FrontendCapabilities): List[String] =
    val live  = AppState.empty.copy(runtime = AppState.empty.runtime.copy(capabilities = capabilities))
    val shown = StartPageTransitions.withStartPageShown(live, live, Nil)
    shown.startPageSurface
      .map(_.content)
      .collect { case SurfaceContent.StartPage(page) => page.actions.map(_.label) }
      .getOrElse(fail("no start page"))

  "Returning to the start page" should "offer one Open... where the frontend has a combined dialog" in {
    returnedTo(FrontendCapabilities.gui.copy(opensFileOrFolder = true)) shouldBe List("New document", "Open...")
  }

  it should "offer Open file and Open folder otherwise" in {
    returnedTo(FrontendCapabilities.gui) shouldBe List("New document", "Open file", "Open folder")
  }

  it should "list the recent folders it is given, after the fixed actions" in {
    val live   = AppState.empty
    val folder = Path.of("/work/book").toAbsolutePath.normalize
    val shown  = StartPageTransitions.withStartPageShown(live, live, Nil, List(folder))

    shown.startPageSurface
      .map(_.content)
      .collect { case SurfaceContent.StartPage(page) => page.actions.map(_.id).last }
      .getOrElse(fail("no start page")) shouldBe s"recent-folder:$folder"
  }
