package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.Escape
import com.serenity.rope.Balance
import com.serenity.state.components.StartupPageComponent
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StartupPageComponentRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "StartupPage Component Routing"

  it should "route to StartupPageComponent when focus is on startup surface" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      // Initialize startup state
      initialState <- AppStartup.initializeState(stateManager, theme, viewportSize)

      // Verify the startup state
      _ = initialState.focus shouldBe Focus.Surface(SurfaceId("surface-0"))
      _ = initialState.startPageSurface should be(defined)

      // Test that the startup component handles the local event contract used by state-manager routing.
      _ =
        val surfaceId = SurfaceId("surface-0")
        val surface   = initialState.surfaceById(surfaceId).get

        val component = surface.content match
          case SurfaceContent.StartPage(_) => new StartupPageComponent()
          case _                           => fail("Expected StartPage content")

        // Test that this component returns Dismiss for Escape
        val result = component.processEvent(Escape, initialState)
        import com.serenity.state.components.ComponentResult
        result shouldBe ComponentResult.Dismiss
    yield ()

    program.unsafeRunSync()
  }
