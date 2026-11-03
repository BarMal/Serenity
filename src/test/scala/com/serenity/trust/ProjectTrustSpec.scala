package com.serenity.trust

import java.nio.file.{Path, Paths}
import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectTrustSpec extends AnyFlatSpec with Matchers:

  private val root: Path     = Paths.get("/work/app").toAbsolutePath
  private val at: Instant    = Instant.parse("2026-11-03T09:00:00Z")
  private val key            = ProjectKey.of(root)
  private val command        = List("rust-analyzer")
  private val bspFile        = root.resolve(".bsp/sbt.json")
  private val bspArgv        = List("java", "-jar", "sbt-launch.jar", "-bsp")
  private val bspSource      = CommandSource.Project(bspFile, sha256 = "aaaa")
  private val trustedProject = ProjectTrust.empty.trust(key, at)

  "ProjectTrust" should "refuse every source, built-in included, for an untrusted root" in {
    for source <- List(CommandSource.BuiltIn, CommandSource.UserLayer, bspSource) do
      ProjectTrust.empty.grant(key, source, command) shouldBe Left(TrustRefusal.ProjectUntrusted(key))
  }

  it should "grant built-in and user-layer commands once the root is trusted" in {
    for source <- List(CommandSource.BuiltIn, CommandSource.UserLayer) do
      val grant = trustedProject.grant(key, source, command)
      grant.map(_.root) shouldBe Right(key)
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

    trustedProject.grant(key, bspSource, bspArgv) shouldBe
      Left(TrustRefusal.CommandNotApproved(key, bspArgv, fingerprint))
    trustedProject.approve(key, fingerprint).grant(key, bspSource, bspArgv).map(_.command) shouldBe Right(bspArgv)
  }

  it should "ask again when the project file or its argv changes" in {
    val approved = trustedProject.approve(key, CommandFingerprint.of(bspSource, bspArgv))
    val edited   = CommandSource.Project(bspFile, sha256 = "bbbb")
    val newArgv  = bspArgv :+ "--debug"

    approved.grant(key, edited, bspArgv).isLeft shouldBe true
    approved.grant(key, bspSource, newArgv).isLeft shouldBe true
  }

  it should "still refuse an approved command once its root is no longer trusted" in {
    val fingerprint = CommandFingerprint.of(bspSource, bspArgv)
    val revoked     = trustedProject.approve(key, fingerprint).revoke(key)

    revoked.grant(key, bspSource, bspArgv) shouldBe Left(TrustRefusal.ProjectUntrusted(key))
    revoked.approved shouldBe empty
  }

  "CommandFingerprint" should "distinguish argv boundaries, not just their concatenation" in {
    CommandFingerprint.of(bspSource, List("a b", "c")) should not be CommandFingerprint.of(bspSource, List("a", "b c"))
  }

  "ProjectKey" should "normalise a path so that `..` cannot alias a different root" in {
    ProjectKey.of(root.resolve("sub/..")) shouldBe key
  }
