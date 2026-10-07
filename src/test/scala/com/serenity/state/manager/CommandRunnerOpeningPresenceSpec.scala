package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.serenity.command.CommandSurfaceItem
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.project.ProjectPresence
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceId}
import com.serenity.ui.presets.UiPresetStore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Opening the command palette does not wait for the project probe (#1911): the palette opens with its presence
  * `Unchecked`, which leaves project commands runnable, and the probe's answer is applied only to the palette that
  * asked for it.
  */
class CommandRunnerOpeningPresenceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val presets = UiPresetStore(Path.of("target", "command-runner-opening-presence-spec.json"))

  private def projectBuild(items: List[CommandSurfaceItem]): Option[CommandSurfaceItem.CommandItem] =
    items.collectFirst { case item: CommandSurfaceItem.CommandItem if item.command.name == "project-build" => item }

  private def rigProbing(gate: Deferred[IO, Unit], found: ProjectPresence): IO[CommandRunnerOpeningRig] =
    CommandRunnerOpeningRig.create(presets, _ => gate.get.as(found))

  "Opening the command palette" should "commit the opening without waiting for the project probe" in {
    val program =
      for
        gate   <- Deferred[IO, Unit]
        rig    <- rigProbing(gate, ProjectPresence.NotDetected)
        _      <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        opened <- rig.state
        items  <- rig.paletteItems
      yield (opened, items)

    val (opened, items) = program.unsafeRunSync()

    opened.runtime.projectPresence shouldBe ProjectPresence.Unchecked
    projectBuild(items).flatMap(_.disabledReason) shouldBe None
  }

  it should "disable the project commands once the probe finds no project" in {
    val program =
      for
        gate  <- Deferred[IO, Unit]
        rig   <- rigProbing(gate, ProjectPresence.NotDetected)
        _     <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        _     <- gate.complete(())
        _     <- rig.operations.awaitEffects
        state <- rig.state
        items <- rig.paletteItems
      yield (state, items)

    val (state, items) = program.unsafeRunSync()

    state.runtime.projectPresence shouldBe ProjectPresence.NotDetected
    projectBuild(items).flatMap(_.disabledReason) shouldBe Some("No project detected.")
  }

  it should "drop the probe's answer when the palette was closed before it arrived" in {
    val program =
      for
        gate   <- Deferred[IO, Unit]
        rig    <- rigProbing(gate, ProjectPresence.NotDetected)
        _      <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        _      <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        closed <- rig.state
        _      <- gate.complete(())
        _      <- rig.operations.awaitEffects
        after  <- rig.state
      yield (closed, after)

    val (closed, after) = program.unsafeRunSync()

    after.runtime.uiSurfaces shouldBe closed.runtime.uiSurfaces
    after.runtime.projectPresence shouldBe ProjectPresence.Unchecked
  }

  "A command palette result" should "not apply to a palette other than the one it was read for" in {
    val program =
      for
        gate   <- Deferred[IO, Unit]
        rig    <- rigProbing(gate, ProjectPresence.Detected)
        _      <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        opened <- rig.state
      yield opened

    val opened  = program.unsafeRunSync()
    val another = SurfaceId("another-palette")

    opened.commandRunnerSurface.map(_.id) should not be Some(another)
    CommandRunnerOpening.withPresenceDetected(opened, another, ProjectPresence.NotDetected) shouldBe opened
    CommandRunnerOpening.withPresetsListed(opened, another, Nil) shouldBe opened
  }

  it should "leave a state without a palette as it is" in {
    val state: AppState = AppState.initial

    state.commandRunnerSurface shouldBe None
    CommandRunnerOpening.withPresenceDetected(state, SurfaceId("palette"), ProjectPresence.Detected) shouldBe state
  }
