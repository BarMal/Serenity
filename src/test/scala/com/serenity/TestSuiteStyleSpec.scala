package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestSuiteStyleSpec extends AnyFlatSpec with Matchers:

  private val testRoot = Paths.get("src", "test", "scala", "com", "serenity")

  "Normal test suites" should "avoid debug-era names and direct console output" in {
    val violations = scalaTestSources.flatMap { path =>
      val relative = testRoot.relativize(path).toString.replace('\\', '/')
      val content  = Files.readString(path)

      List(
        Option.when(relative.contains("Debug"))(s"$relative uses a debug-era file name"),
        Option.when(DebugSuitePattern.findFirstIn(content).nonEmpty)(s"$relative uses a debug-era suite name"),
        Option.when(!ChildProcessMains(relative) && ConsoleOutputPattern.findFirstIn(content).nonEmpty)(
          s"$relative writes to stdout during normal tests"
        ),
        Option.when(DisabledAnnotationPattern.findFirstIn(content).nonEmpty)(
          s"$relative disables a normal test suite"
        ),
        Option.when(DisabledTestCallPattern.findFirstIn(content).nonEmpty)(
          s"$relative disables a normal test case"
        )
      ).flatten
    }

    violations shouldBe Nil
  }

  it should "block a thread with a wall-clock sleep only where the sleep is the behaviour under test" in {
    val sleepers = scalaTestSources.flatMap { path =>
      val relative = testRoot.relativize(path).toString.replace('\\', '/')
      Option.when(ThreadSleepPattern.findFirstIn(Files.readString(path)).nonEmpty)(relative)
    }.toSet

    val unexpected = sleepers -- ThreadSleepAllowlist.keySet
    val stale      = ThreadSleepAllowlist.keySet -- sleepers

    withClue(
      s"new thread sleeps (wait on a Deferred/latch/awaitEffects, or use TestControl; allowlist only a real-time boundary): $unexpected; "
    )(unexpected shouldBe Set.empty)
    withClue(s"allowlisted files that no longer sleep, remove them from the allowlist: $stale; ")(
      stale shouldBe Set.empty
    )
  }

  private def scalaTestSources: List[Path] =
    Using.resource(Files.walk(testRoot)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path))
        .filter(path => path.toString.endsWith(".scala"))
        .filterNot(path => testRoot.relativize(path).toString.replace('\\', '/').startsWith("perf/"))
        .toList
    }

  // Entry points a spec launches as a separate JVM: their stdout is the channel the parent spec reads, not test noise.
  private val ChildProcessMains = Set("crash/KillRecoveryChild.scala")

  private val DebugSuitePattern         = (raw"\bclass\s+" + "Deb" + "ug" + raw"\w*Sp" + "ec" + raw"\b").r
  private val ConsoleOutputPattern      = ("println" + raw"\s*\(").r
  private val DisabledAnnotationPattern = ("@" + "Ignore").r
  private val DisabledTestCallPattern   = (raw"\b" + "ignore" + raw"\s*\(").r

  private val ThreadSleepPattern = ("Thread" + raw"\.sleep\s*\(").r

  // Each entry is a sleep that is the stimulus or the bound itself, not a wait for something else to happen.
  private val ThreadSleepAllowlist = Map(
    "app/instance/InstanceLockSpec.scala"         -> "child JVM parks forever so the lock stays held",
    "spellcheck/DictionaryCacheSharingSpec.scala" -> "simulates a slow dictionary build the sharing must not repeat",
    "testkit/StallSampler.scala"                  -> "production default for the injected sleep",
    "testkit/StallSamplerSpec.scala"              -> "yields the sampler thread once its scripted delays run out",
    "testkit/RuntimeShutdownWatch.scala"          -> "the watcher's own sampling interval on a daemon thread",
    "io/MainThreadHandoffSpec.scala"              -> "work that overruns the handoff's start timeout"
  )
