package com.serenity.config

/** The settings that are read once at startup, so changing them in the file changes nothing until Serenity restarts.
  *
  * Reloading applies every other setting live; these are applied to the stored config too, so the file and the editor
  * agree about what is set, and the user is told why nothing moved.
  */
object ConfigRestart:

  private val startupOnly: List[(String, AppConfig => Any)] = List(
    "window.chrome"                  -> (_.windowChromeMode),
    "window.preferred.width/height"  -> (_.preferredWindowSize),
    "lsp.* language server settings" -> (_.languageToolsConfig.lspUserConfig),
    "startup.warm_up"                -> (_.surfaceConfig.startupWarmUpEnabled)
  )

  /** The startup-only settings `after` sets differently from `before`. */
  def changed(before: AppConfig, after: AppConfig): List[String] =
    startupOnly.collect { case (name, read) if read(before) != read(after) => name }

  def notice(changed: List[String]): Option[String] =
    Option.when(changed.nonEmpty)(s"${changed.mkString(", ")} only take effect after restarting Serenity.")
