package com.serenity.command

import com.serenity.project.{ProjectPresence, ProjectTaskKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandPrerequisitesSpec extends AnyFlatSpec with Matchers:

  private def command(intent: CommandIntent): Command =
    Command.typed("under-test", "Under test", intent, CommandCategory.Project)

  private val runBuild = command(CommandIntent.Project(ProjectIntent.RunProjectTask(ProjectTaskKind.Build)))

  private def unmet(command: Command, presence: ProjectPresence): Option[String] =
    CommandPrerequisites.unmetReason(command, CommandRunnerContext(projectPresence = presence))

  "CommandPrerequisites.unmetReason" should "refuse a project task when no project encloses the working file" in
    ProjectTaskKind.values.foreach { kind =>
      unmet(command(CommandIntent.Project(ProjectIntent.RunProjectTask(kind))), ProjectPresence.NotDetected) shouldBe
        Some("No project detected.")
    }

  it should "leave a project task runnable once a project is detected" in {
    unmet(runBuild, ProjectPresence.Detected) shouldBe None
  }

  it should "leave a project task runnable before detection has run, rather than greying it on a guess" in {
    unmet(runBuild, ProjectPresence.Unchecked) shouldBe None
  }

  it should "never block cancelling a running task" in {
    unmet(command(CommandIntent.Project(ProjectIntent.CancelProjectTask)), ProjectPresence.NotDetected) shouldBe None
  }

  it should "not affect commands outside the project family" in {
    unmet(command(CommandIntent.Edit(EditIntent.Undo)), ProjectPresence.NotDetected) shouldBe None
  }
