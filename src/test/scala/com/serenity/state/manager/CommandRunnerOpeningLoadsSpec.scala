package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceContent}
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Opening the command palette commits the opening first (#1911): the preset index is read on its lane and comes back
  * as a result applied only while that palette is still the one waiting for it.
  */
class CommandRunnerOpeningLoadsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val preset = UiPreset(name = "Custom", config = AppConfig.default, themeName = Theme.light.name)

  /** `list` runs until the gate opens, as a store on a slow or network disk would. */
  final private class GatedPresetStore(gate: Deferred[IO, Unit], listings: Ref[IO, Int])
      extends UiPresetStore(Path.of("target", "command-runner-opening-loads-spec.json")):
    override def list(): IO[List[UiPreset]] = listings.update(_ + 1) >> gate.get.as(List(preset))

  private def previewNames(state: AppState): List[String] =
    state.commandRunnerSurface
      .map(_.content)
      .collect { case SurfaceContent.CommandPalette(runner) => runner.uiPresetPreviews.map(_.name) }
      .getOrElse(Nil)

  private def rigWithGatedStore: IO[(CommandRunnerOpeningRig, Deferred[IO, Unit])] =
    for
      gate     <- Deferred[IO, Unit]
      listings <- Ref.of[IO, Int](0)
      rig      <- CommandRunnerOpeningRig.create(new GatedPresetStore(gate, listings))
    yield (rig, gate)

  "Opening the command palette" should "commit the opening without waiting for the preset index to be read" in {
    val program =
      for
        (rig, _) <- rigWithGatedStore
        _        <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        opened   <- rig.state
      yield opened

    val opened = program.unsafeRunSync()

    opened.commandRunnerSurface.isDefined shouldBe true
    previewNames(opened) shouldBe Nil
  }

  it should "show the presets once the index has been read" in {
    val program =
      for
        (rig, gate) <- rigWithGatedStore
        _           <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        _           <- gate.complete(())
        _           <- rig.operations.awaitEffects
        listed      <- rig.state
      yield listed

    previewNames(program.unsafeRunSync()) shouldBe List("Custom")
  }

  it should "drop the listing when the palette was closed before it arrived" in {
    val program =
      for
        (rig, gate) <- rigWithGatedStore
        _           <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        _           <- rig.pipeline.applyEvent(ToggleCommandRunner).timeout(5.seconds)
        closed      <- rig.state
        _           <- gate.complete(())
        _           <- rig.operations.awaitEffects
        after       <- rig.state
      yield (closed, after)

    val (closed, after) = program.unsafeRunSync()

    after.commandRunnerSurface shouldBe None
    after.runtime.uiSurfaces shouldBe closed.runtime.uiSurfaces
  }
