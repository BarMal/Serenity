package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.reducers.*

/** Interprets reducer effects through the focused handlers supplied by the composition root. */
final private[manager] class CommandEffectInterpreter(
    dependencies: CommandEffectInterpreter.Dependencies
):

  def interpret(effect: AppEffect): IO[Unit] =
    effect match
      case AppEffect.Lifecycle(value)      => dependencies.lifecycle(value)
      case AppEffect.CommandRequest(value) => dependencies.command(value)
      case AppEffect.Theme(value)          => dependencies.theme(value)
      case AppEffect.Surface(value)        => dependencies.surface(value)
      case AppEffect.File(value)           => dependencies.file(value)
      case AppEffect.Explorer(value)       => dependencies.explorer(value)
      case AppEffect.Workflow(value)       => dependencies.workflow(value)
      case AppEffect.LspQueue(value)       => dependencies.lspQueue(value)
      case AppEffect.Animation(value)      => dependencies.animation(value)

private[manager] object CommandEffectInterpreter:

  final case class Dependencies(
      lifecycle: LifecycleEffect => IO[Unit],
      command: CommandEffect => IO[Unit],
      theme: ThemeEffect => IO[Unit],
      surface: SurfaceEffect => IO[Unit],
      file: FileEffect => IO[Unit],
      explorer: ExplorerEffect => IO[Unit],
      workflow: WorkflowEffect => IO[Unit],
      lspQueue: LspQueueEffect => IO[Unit],
      animation: AnimationEffect => IO[Unit]
  )
