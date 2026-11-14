package com.serenity.app

import java.nio.file.Path

/** The launches that type into the editor must not run against someone's real files: the start-up probe and a renderer
  * trial. What stops them, if anything.
  */
object LaunchRefusal:

  def apply(options: LaunchOptions, userHome: Path = StartupProbe.userHome): Option[String] =
    StartupProbe.refusal(options, userHome).orElse(RendererTrialChild.refusal(options))
