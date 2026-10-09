package com.serenity.command

import com.serenity.lsp.config.LanguageId
import com.serenity.project.ProjectPresence
import com.serenity.state.models.EditingContext

/** What the command runner needs to know about the running session beyond `AppConfig`: the values its pickers show as
  * current, the catalogs they pick from, and what decides which commands are offered or can run. Captured when the
  * runner is activated, like `capabilities`.
  */
final case class CommandRunnerContext(
    bufferLanguage: Option[LanguageId] = None,
    themeNames: List[String] = Nil,
    currentThemeName: Option[String] = None,
    editingContext: Option[EditingContext] = None,
    projectPresence: ProjectPresence = ProjectPresence.Unchecked,
    opensFileOrFolder: Boolean = false
)

object CommandRunnerContext:
  val empty: CommandRunnerContext = CommandRunnerContext()
