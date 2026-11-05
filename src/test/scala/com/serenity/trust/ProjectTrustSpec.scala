package com.serenity.trust

import java.nio.file.{Path, Paths}
import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectTrustSpec extends AnyFlatSpec with Matchers:

  private val root: Path                       = Paths.get("/work/app").toAbsolutePath
  private val at: Instant                      = Instant.parse("2026-11-03T09:00:00Z")
  private val projectKey                       = ProjectKey.of(root)
  private val command                          = List("rust-analyzer")
  private val bspFile                          = root.resolve(".bsp/sbt.json")
  private val bspArgv                          = List("java", "-jar", "sbt-launch.jar", "-bsp")
  private val bspSource: CommandSource.Project = CommandSource.Project(bspFile, sha256 = "aaaa")
  private val trustedProject                   = ProjectTrust.empty.trust(projectKey, at)

  "ProjectTrust" should "refuse every source, built-in included, for an untrusted root" in {
    for source <- List(CommandSource.BuiltIn, CommandSource.UserLayer, bspSource) do
      ProjectTrust.empty.grant(projectKey, source, command) shouldBe Left(TrustRefusal.ProjectUntrusted(projectKey))
  }

  it should "grant built-in and user-layer commands once the root is trusted" in {
    for source <- List(CommandSource.BuiltIn, CommandSource.UserLayer) do
      val grant = trustedProject.grant(projectKey, source, command)
      grant.map(_.root) shouldBe Right(projectKey)
      grant.map(_.command) shouldBe Right(command)
  }

  it should "cover a descendant of a trusted root, but not a sibling or an ancestor" in {
    val child   = ProjectKey.of(root.resolve("modules/core"))
    val sibling = ProjectKey.of(Paths.get("/work/app-other").toAbsolutePath)
    val parent  = ProjectKey.of(Paths.get("/work").toAbsolutePath)

    trustedProject.isTrusted(child) shouldBe true
    trustedProject.isTrusted(sibling) shouldBe false
    trustedProject.isTrusted(parent) shouldBe false
  }

  it should "refuse a project-sourced command until its fingerprint is approved" in {
    val fingerprint = CommandFingerprint.of(bspSource, bspArgv)

    trustedProject.grant(projectKey, bspSource, bspArgv) shouldBe
      Left(TrustRefusal.CommandNotApproved(projectKey, bspArgv, fingerprint))
    trustedProject.approve(projectKey, fingerprint).grant(projectKey, bspSource, bspArgv).map(_.command) shouldBe Right(
      bspArgv
    )
  }

  it should "ask again when the project file or its argv changes" in {
    val approved              = trustedProject.approve(projectKey, CommandFingerprint.of(bspSource, bspArgv))
    val edited: CommandSource = CommandSource.Project(bspFile, sha256 = "bbbb")
    val newArgv               = bspArgv :+ "--debug"

    approved.grant(projectKey, edited, bspArgv).isLeft shouldBe true
    approved.grant(projectKey, bspSource, newArgv).isLeft shouldBe true
  }

  it should "still refuse an approved command once its root is no longer trusted" in {
    val fingerprint = CommandFingerprint.of(bspSource, bspArgv)
    val revoked     = trustedProject.approve(projectKey, fingerprint).revoke(projectKey)

    revoked.grant(projectKey, bspSource, bspArgv) shouldBe Left(TrustRefusal.ProjectUntrusted(projectKey))
    revoked.approved shouldBe empty
  }

  "CommandFingerprint" should "distinguish argv boundaries, not just their concatenation" in {
    CommandFingerprint.of(bspSource, List("a b", "c")) should not be CommandFingerprint.of(bspSource, List("a", "b c"))
  }

  "ProjectKey" should "normalise a path so that `..` cannot alias a different root" in {
    ProjectKey.of(root.resolve("sub/..")) shouldBe projectKey
  }
