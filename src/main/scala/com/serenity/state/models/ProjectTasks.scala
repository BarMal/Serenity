package com.serenity.state.models

import com.serenity.project.ProjectTaskCommand

/** The project task whose output the terminal panel shows (#1697). `nextId` versions task results: output or a finish
  * posted for any id but the running one's is dropped. `terminalText` is what the project output panel shows -- kept
  * whether or not the panel is docked, so hiding the panel loses nothing and showing it again picks up where it is.
  */
final case class ProjectTasks(
    nextId: Long = 0L,
    running: Option[RunningProjectTask] = None,
    terminalText: String = ""
)

/** `output` is the bounded tail the terminal panel renders, as `ProjectTaskRunner.appendOutputTail` keeps it. */
final case class RunningProjectTask(id: Long, command: ProjectTaskCommand, output: String)
