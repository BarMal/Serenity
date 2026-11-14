package com.serenity.app

import cats.effect.{ExitCode, IO}

/** The launches that do one job and quit without starting the editor: the version banner and the renderer optimizer.
  */
object OneShot:

  def unapply(options: LaunchOptions): Option[IO[ExitCode]] =
    if options.showVersion then Some(IO(println(VersionBanner.current)).as(ExitCode.Success))
    else if options.optimizeRenderer then Some(RendererOptimizeProduction.run(options))
    else None
