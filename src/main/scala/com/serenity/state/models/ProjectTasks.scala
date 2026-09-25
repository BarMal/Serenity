package com.serenity.state.models

import com.serenity.project.ProjectTaskCommand

/** The project task whose output the terminal panel shows (#1697). `nextId` versions task results: output or a finish
  * posted for any id but the running one's is dropped.
  */
final case class ProjectTasks(nextId: Long = 0L, running: Option[RunningProjectTask] = None)

/** `output` is the bounded tail the terminal panel renders, as `ProjectTaskRunner.appendOutputTail` keeps it. */
final case class RunningProjectTask(id: Long, command: ProjectTaskCommand, output: String)
