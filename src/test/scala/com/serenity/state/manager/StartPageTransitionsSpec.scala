package com.serenity.state.manager

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
