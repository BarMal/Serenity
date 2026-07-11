package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerCapabilitySpec extends AnyFlatSpec with Matchers:

  "RenderController" should "depend only on the event application capability" in {
    val applied = Ref.of[IO, List[ResizeEvent]](Nil).unsafeRunSync()
    val events = new EventApplier:
      def applyEvent(event: com.serenity.keystroke.events.Event): IO[Unit] =
        event match
          case resize: ResizeEvent => applied.update(_ :+ resize)
          case _                   => IO.unit

    RenderController.handleResize(Some(ViewportSize(120, 40)), events, IO.unit).unsafeRunSync()

    applied.get.unsafeRunSync() shouldBe List(ResizeEvent(ViewportSize(120, 40)))
  }
