package com.serenity.command

import com.serenity.keystroke.events.Direction
import com.serenity.project.ProjectTaskKind
import com.serenity.state.models.{PanelId, PanelRegistry}

/** Panel, pane and project-task commands. Split out of `CommandRegistry.defaultCommands` to keep both under the
  * architecture size targets -- see that method's doc.
  */
private[command] object CommandRegistryPanelProjectCommands:

  /** One show/hide and one focus command per registered panel (none to focus a panel that can't take focus), plus one
    * to maximise or restore the focused panel -- generated from the registry, so a new panel gets them by registering.
    */
  private[command] def panelCommands: List[Command] =
    PanelId.values.toList.flatMap { id =>
      val registration = PanelRegistry.registrationFor(id)
      Command.typed(
        s"toggle-${id.key}-panel",
        s"Show or hide the ${registration.label} panel. ${registration.description}",
        CommandIntent.View(ViewIntent.TogglePanelShown(id)),
        CommandCategory.View,
        label = s"Show/Hide ${registration.label}"
      ) :: Option
        .when(registration.focusable)(
          Command.typed(
            s"focus-${id.key}-panel",
            s"Focus the ${registration.label} panel, showing it if it is hidden.",
            CommandIntent.View(ViewIntent.FocusPanel(id)),
            CommandCategory.View,
            label = s"Focus ${registration.label}"
          )
        )
        .toList
    } :+ Command.typed(
      "toggle-maximise-panel",
      "Maximise the focused panel into the workspace, or restore the maximised panel.",
      CommandIntent.View(ViewIntent.ToggleMaximisePanel),
      CommandCategory.View,
      label = "Maximise/Restore Panel"
    ) :++ Direction.values.map { direction =>
      Command.typed(
        s"focus-${direction.toString.toLowerCase}",
        s"Move focus to the editor pane or panel ${direction.toString.toLowerCase} of the focused one.",
        CommandIntent.View(ViewIntent.FocusInDirection(direction)),
        CommandCategory.View,
        label = s"Focus $direction"
      )
    } :+ CommandRunnerSettingsPanelItems.arrangePanelsCommand

  private[command] def paneCommands: List[Command] = List(
    Command.typed(
      "split-pane-horizontal",
      "Split the focused editor pane horizontally, carrying its buffer into the new pane.",
      CommandIntent.View(ViewIntent.SplitPaneHorizontal),
      CommandCategory.View,
      label = "Split Pane Horizontally"
    ),
    Command.typed(
      "split-pane-vertical",
      "Split the focused editor pane vertically, carrying its buffer into the new pane.",
      CommandIntent.View(ViewIntent.SplitPaneVertical),
      CommandCategory.View,
      label = "Split Pane Vertically"
    ),
    Command.typed(
      "open-chapter-note",
      "Open the note for the chapter the cursor is in beside the manuscript, creating it the first time.",
      CommandIntent.View(ViewIntent.OpenChapterNote),
      CommandCategory.View,
      label = "Open Chapter Note"
    ),
    Command.typed(
      "open-term-note",
      "Open the note for the term under the cursor, or the one selected, beside the manuscript.",
      CommandIntent.View(ViewIntent.OpenKeywordNote),
      CommandCategory.View,
      label = "Open Term Note"
    ),
    Command.typed(
      "toggle-chapter-ghosts",
      "Show or hide the faded overview under empty chapters. The notes themselves are untouched.",
      CommandIntent.View(ViewIntent.ToggleChapterGhosts),
      CommandCategory.View,
      label = "Show/Hide Chapter Ghosts"
    ),
    Command.typed(
      "toggle-notes-pin",
      "Pin the notes pane to the note it shows, or let it follow the chapter the cursor is in again.",
      CommandIntent.View(ViewIntent.ToggleNotesPin),
      CommandCategory.View,
      label = "Pin/Unpin Notes"
    ),
    Command.typed(
      "close-pane",
      "Close the focused editor pane.",
      CommandIntent.View(ViewIntent.ClosePane),
      CommandCategory.View,
      label = "Close Pane"
    )
  )

  private[command] def projectCommands: List[Command] = List(
    Command.typed(
      "project-build",
      "Build the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Build)),
      CommandCategory.Project,
      label = "Build Project"
    ),
    Command.typed(
      "project-test",
      "Run tests for the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Test)),
      CommandCategory.Project,
      label = "Test Project"
    ),
    Command.typed(
      "project-run",
      "Run the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Run)),
      CommandCategory.Project,
      label = "Run Project"
    ),
    Command.typed(
      "project-debug",
      "Launch the detected project through its debug task.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Debug)),
      CommandCategory.Project,
      label = "Run Debug Task"
    ),
    Command.typed(
      "project-dependencies",
      "Show or resolve dependencies for the detected project.",
      CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Dependencies)),
      CommandCategory.Project,
      label = "Project Dependencies"
    ),
    Command.typed(
      "project-cancel",
      "Cancel the running project task.",
      CommandIntent.Project(ProjectIntent.CancelProjectTask),
      CommandCategory.Project,
      label = "Cancel Project Task"
    )
  )
