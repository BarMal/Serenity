package com.serenity.diagnostics

import java.nio.file.Path

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogFolderSpec extends AnyFlatSpec with Matchers:

  private val folder = Path.of(System.getProperty("java.io.tmpdir"), "serenity-logs")

  "LogFolder.openCommand" should "ask Explorer on Windows" in {
    LogFolder.openCommand(Platform.Windows, folder) shouldBe List("explorer.exe", folder.toString)
  }

  it should "ask open on macOS" in {
    LogFolder.openCommand(Platform.MacOs, folder) shouldBe List("open", folder.toString)
  }

  it should "ask xdg-open elsewhere" in {
    LogFolder.openCommand(Platform.Other, folder) shouldBe List("xdg-open", folder.toString)
  }
