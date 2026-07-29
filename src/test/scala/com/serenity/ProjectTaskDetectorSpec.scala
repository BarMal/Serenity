package com.serenity

import java.nio.file.{Files, Path, Paths}

import com.serenity.project.{ProjectTaskCommand, ProjectTaskDetector, ProjectTaskKind, ProjectTaskTerminal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectTaskDetectorSpec extends AnyFlatSpec with Matchers:

  private def withTempDirectory[A](prefix: String)(use: Path => A): A =
    val root = Files.createTempDirectory(prefix)
    try use(root)
    finally deleteRecursively(root)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try
        stream.toArray.toList.map(_.asInstanceOf[Path]).sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
      finally stream.close()

  "ProjectTaskDetector" should "resolve sbt tasks from the nearest build.sbt root" in withTempDirectory("sbt-project") {
    root =>
      val nested = Files.createDirectories(root.resolve("src").resolve("main").resolve("scala"))
      val file   = nested.resolve("Main.scala")
      Files.writeString(root.resolve("build.sbt"), "scalaVersion := \"3.5.0\"")
      Files.writeString(file, "object Main")

      val task = ProjectTaskDetector.detect(file, ProjectTaskKind.Test)

      task.map(_.workingDirectory) shouldBe Some(root)
      task.map(_.commandLine) shouldBe Some(List("sbt", "test"))
      task.map(_.ecosystemLabel) shouldBe Some("sbt")
  }

  it should "prefer the nearest supported marker when projects are nested" in withTempDirectory("nested-project") {
    root =>
      val nested = Files.createDirectories(root.resolve("tool"))
      Files.writeString(root.resolve("build.sbt"), "scalaVersion := \"3.5.0\"")
      Files.writeString(nested.resolve("package.json"), """{"scripts":{"test":"vitest"}}""")

      val task = ProjectTaskDetector.detect(nested.resolve("index.ts"), ProjectTaskKind.Build)

      task.map(_.workingDirectory) shouldBe Some(nested)
      task.map(_.commandLine) shouldBe Some(List("npm", "run", "build"))
      task.map(_.ecosystemLabel) shouldBe Some("npm")
  }

  it should "resolve dependency workflows for supported project ecosystems" in withTempDirectory("cargo-project") {
    root =>
      Files.writeString(root.resolve("Cargo.toml"), "[package]\nname = \"demo\"\nversion = \"0.1.0\"")

      val task = ProjectTaskDetector.detect(root, ProjectTaskKind.Dependencies)

      task.map(_.commandLine) shouldBe Some(List("cargo", "tree"))
      task.map(_.workingDirectory) shouldBe Some(root)
  }

  it should "return no task when no supported project marker exists" in withTempDirectory("plain-directory") { root =>
    ProjectTaskDetector.detect(root.resolve("notes.txt"), ProjectTaskKind.Build) shouldBe None
  }

  it should "describe debug execution as a task in terminal status text" in {
    val command = ProjectTaskCommand(ProjectTaskKind.Debug, "sbt", Paths.get("."), "sbt", List("run"))

    ProjectTaskTerminal.started(command) should include("Running debug task")
    ProjectTaskTerminal.failedToStart(command, RuntimeException("unavailable")) should include(
      "Failed to start debug task"
    )
  }
