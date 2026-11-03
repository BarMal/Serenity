package com.serenity.trust

import java.nio.file.{Files, Path}
import java.time.Instant

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectTrustStoreSpec extends AnyFlatSpec with Matchers:

  private def withTempDirectory[A](body: Path => A): A =
    val directory = Files.createTempDirectory("project-trust-store")
    try body(directory)
    finally
      Files
        .walk(directory)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(path => Files.deleteIfExists(path): Unit)

  private val at = Instant.parse("2026-11-03T09:00:00Z")

  "ProjectTrustStore" should "read a missing file as no trust at all" in withTempDirectory { directory =>
    ProjectTrustStore.load(directory.resolve("trust.conf")).unsafeRunSync() shouldBe Right(ProjectTrust.empty)
  }

  it should "round-trip trusted roots and approvals, including paths that need quoting" in withTempDirectory {
    directory =>
      val file                          = directory.resolve("trust.conf")
      val rootA                         = ProjectKey.of(directory.resolve("app \"one\""))
      val rootB                         = ProjectKey.of(directory.resolve("lib"))
      val source: CommandSource.Project = CommandSource.Project(directory.resolve("lib/.bsp/sbt.json"), sha256 = "abc")
      val trust = ProjectTrust.empty
        .trust(rootA, at)
        .trust(rootB, at.plusSeconds(60))
        .approve(rootB, CommandFingerprint.of(source, List("sbt", "-bsp")))

      (ProjectTrustStore.save(file, trust) >> ProjectTrustStore.load(file)).unsafeRunSync() shouldBe Right(trust)
  }

  it should "fail closed on a malformed file, reporting why rather than granting anything" in withTempDirectory {
    directory =>
      val file = directory.resolve("trust.conf")
      Files.writeString(file, "trusted = [ { root = 42 } ]")

      ProjectTrustStore.load(file).unsafeRunSync().isLeft shouldBe true
  }

  it should "refuse a file written by a newer version instead of guessing at its meaning" in withTempDirectory {
    directory =>
      val file = directory.resolve("trust.conf")
      Files.writeString(file, "version = 99\ntrusted = []\napproved = []")

      ProjectTrustStore.load(file).unsafeRunSync().left.map(_.contains("version 99")) shouldBe Left(true)
  }

  "ProjectKey.resolve" should "key a symlinked root by its real path" in withTempDirectory { directory =>
    val real = Files.createDirectory(directory.resolve("real"))
    val link = Files.createSymbolicLink(directory.resolve("link"), real)

    ProjectKey.resolve(link).unsafeRunSync() shouldBe ProjectKey.resolve(real).unsafeRunSync()
  }
