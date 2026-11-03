package com.serenity.trust

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectTrustServiceSpec extends AnyFlatSpec with Matchers:

  private def withTempDirectory[A](body: Path => A): A =
    val directory = Files.createTempDirectory("project-trust-service")
    try body(directory)
    finally
      Files
        .walk(directory)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(path => Files.deleteIfExists(path): Unit)

  "ProjectTrustService" should "start from what the store holds" in withTempDirectory { directory =>
    val file = directory.resolve("trust.conf")
    val root = ProjectKey.of(directory.resolve("app"))
    val held = ProjectTrust.empty.trust(root, java.time.Instant.EPOCH)

    val loaded = (ProjectTrustStore.save(file, held) >> ProjectTrustService.load(file)).unsafeRunSync()

    loaded.problem shouldBe None
    loaded.service.current.unsafeRunSync() shouldBe held
  }

  it should "trust a folder at once and keep it across a reload" in withTempDirectory { directory =>
    val file = directory.resolve("trust.conf")
    val root = ProjectKey.of(directory.resolve("app"))

    val (now, reloaded) = (for
      loaded   <- ProjectTrustService.load(file)
      _        <- loaded.service.trustFolder(root)
      now      <- loaded.service.current
      reloaded <- ProjectTrustService.load(file).flatMap(_.service.current)
    yield (now, reloaded)).unsafeRunSync()

    now.isTrusted(root) shouldBe true
    reloaded.isTrusted(root) shouldBe true
  }

  it should "trust nothing when the store cannot be read, and say why" in withTempDirectory { directory =>
    val file = directory.resolve("trust.conf")
    Files.writeString(file, "this is { not hocon")

    val loaded = ProjectTrustService.load(file).unsafeRunSync()

    loaded.problem.isDefined shouldBe true
    loaded.service.current.unsafeRunSync() shouldBe ProjectTrust.empty
  }
