package com.serenity.state.models

import com.serenity.command.Command
import com.serenity.config.AppConfig
import com.serenity.ui.theme.Theme

/** A setting value the command runner is previewing. The previewed value is live in [[Persisted]], but the config and
  * theme from before the preview began are kept here and are what gets saved until the preview is committed, so a
  * preview never reaches disk. `scope` names the row or picker the preview belongs to; `previewed` is the command that
  * produced the live value.
  */
final case class PendingSetting(
    scope: String,
    previewed: Command,
    committedConfig: AppConfig,
    committedTheme: Theme
)
