package com.serenity.command

import com.serenity.project.ProjectTaskKind
import com.serenity.ui.layout.PanelPosition

/** Pinned-panel focus/expand/unpin commands and project-task commands. Split out of `CommandRegistry.defaultCommands`
  * to keep both under the architecture size targets -- see that method's doc.
  */
private[command] object CommandRegistryPanelProjectCommands:

  private[command] def panelFocusCommands: List[Command] = List(
Command.typed(
  "focus-left-panel",
  "Focus the left pinned panel.",
  CommandIntent.View(ViewIntent.FocusPanel(PanelPosition.Left)),
  CommandCategory.View,
  label = "Focus Left Panel"
),
Command.typed(
  "focus-right-panel",
  "Focus the right pinned panel.",
  CommandIntent.View(ViewIntent.FocusPanel(PanelPosition.Right)),
  CommandCategory.View,
  label = "Focus Right Panel"
),
Command.typed(
  "focus-bottom-panel",
  "Focus the bottom pinned panel.",
  CommandIntent.View(ViewIntent.FocusPanel(PanelPosition.Bottom)),
  CommandCategory.View,
  label = "Focus Bottom Panel"
),
Command.typed(
  "unpin-left-panel",
  "Unpin the left panel.",
  CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Left)),
  CommandCategory.View,
  label = "Unpin Left Panel"
),
Command.typed(
  "unpin-right-panel",
  "Unpin the right panel.",
  CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Right)),
  CommandCategory.View,
  label = "Unpin Right Panel"
),
Command.typed(
  "unpin-bottom-panel",
  "Unpin the bottom panel.",
  CommandIntent.View(ViewIntent.UnpinPanel(PanelPosition.Bottom)),
  CommandCategory.View,
  label = "Unpin Bottom Panel"
),
Command.typed(
  "expand-left-panel",
  "Expand the left pinned panel into the editor workspace.",
  CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Left)),
  CommandCategory.View,
  label = "Expand Left Panel"
),
Command.typed(
  "expand-right-panel",
  "Expand the right pinned panel into the editor workspace.",
  CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Right)),
  CommandCategory.View,
  label = "Expand Right Panel"
),
Command.typed(
  "expand-bottom-panel",
  "Expand the bottom pinned panel into the editor workspace.",
  CommandIntent.View(ViewIntent.ExpandPanel(PanelPosition.Bottom)),
  CommandCategory.View,
  label = "Expand Bottom Panel"
),
Command.typed(
  "collapse-expanded-panel",
  "Collapse the expanded panel back to its pinned position.",
  CommandIntent.View(ViewIntent.CollapseExpandedPanel),
  CommandCategory.View,
  label = "Collapse Expanded Panel"
),
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

