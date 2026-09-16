package com.serenity.command

import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.EditingContext

/** What the settings tree needs to know about the running session beyond `AppConfig`: the values its pickers show as
  * current and the catalogs they pick from. Captured when the runner is activated, like `isTuiMode`.
  */
final case class CommandRunnerContext(
    bufferLanguage: Option[LanguageId] = None,
    themeNames: List[String] = Nil,
    currentThemeName: Option[String] = None,
    editingContext: Option[EditingContext] = None
)

object CommandRunnerContext:
  val empty: CommandRunnerContext = CommandRunnerContext()
