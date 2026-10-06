package com.serenity.crash

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.Try

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.io.FileManager
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, UnreadableSession}
import com.serenity.state.manager.HotExitRecovery
import com.serenity.state.models.{AppState, Buffer, BufferId}
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Release gate: a hard kill (SIGKILL, power loss) of a real Serenity-code JVM in the middle of saving never loses
  * saved files or hot-exit text, and never leaves a session that blocks the next start.
  *
  * [[KillRecoveryChild]] runs in a separate JVM and saves through the production code; each round trip is
  * `Process.destroyForcibly` at a different point, then the survivors are read back with the production loading code. A
  * save the child reported finished must be on disk; the one it was in the middle of may or may not have landed, but
  * only as a whole.
  */
class KillRecoverySpec extends AnyFlatSpec with Matchers with OptionValues:

  import KillRecoveryPayload.*

  private given Balance = Balance.default

  private val Rounds = 12

  /** What a restart found after one kill. */
  final private case class Recovery(
      round: Int,
      fileAck: Int,
      sessionAck: Int,
      file: Either[Throwable, String],
      startup: Either[Throwable, Startup]
  )

  final private case class Startup(
      setAside: Option[UnreadableSession],
      loaded: Option[AppState],
      offered: Boolean,
      savesAgain: Boolean
  ):
    def hotBuffer: Option[Buffer] = loaded.flatMap(_.persisted.buffers.values.headOption)

  private lazy val recoveries: List[Recovery] = (1 to Rounds).toList.map(killAndRecover)

  "A hard-killed Serenity" should "leave the saved file as a complete previous or complete new version" in
    recoveries.foreach { recovery =>
      val found = withClue(s"round ${recovery.round}: ")(recovery.file.fold(throw _, identity))
      withClue(s"round ${recovery.round} (file ack ${recovery.fileAck}, ${found.length} chars): ") {
        versionOf(FileKind, found).value should be >= recovery.fileAck
      }
    }

  it should "start again: the session loads, or is set aside, and a fresh save still works" in
    recoveries.foreach { recovery =>
      val startup = withClue(s"round ${recovery.round} startup: ")(recovery.startup.fold(throw _, identity))
      withClue(s"round ${recovery.round}: ") {
        startup.setAside shouldBe None
        startup.savesAgain shouldBe true
        if recovery.sessionAck > 0 then startup.loaded should not be empty
      }
    }

  it should "bring back the hot-exit text of the last completed session write, offered for recovery" in
    recoveries.foreach { recovery =>
      val startup = withClue(s"round ${recovery.round} startup: ")(recovery.startup.fold(throw _, identity))
      startup.hotBuffer.foreach { buffer =>
        val text = buffer.document.content.collect()
        withClue(s"round ${recovery.round} (session ack ${recovery.sessionAck}, ${text.length} chars): ") {
          versionOf(HotKind, text).value should be >= recovery.sessionAck
          buffer.document.isDirty shouldBe true
          HotExitRecovery.holdsBackup(buffer) shouldBe true
          startup.offered shouldBe true
        }
      }
    }

  it should "have been killed at varied points, mid-write rather than between writes" in {
    recoveries.map(r => (r.fileAck, r.sessionAck)).distinct.size should be > Rounds / 2
    recoveries.count(r => r.fileAck != r.sessionAck) should be > 0
    recoveries.count(r => r.fileAck == r.sessionAck) should be > 0
  }

  private def killAndRecover(round: Int): Recovery =
    val directory   = Files.createTempDirectory("kill-recovery")
    val sessionRoot = directory.resolve("home")
    val target      = directory.resolve("target.txt")
    Files.writeString(target, text(FileKind, 0))
    try
      val errors = directory.resolve("child.err")
      val acks   = killedAfter(round, childOf(sessionRoot, target, errors), errors)
      Recovery(round, acks.fileAck, acks.sessionAck, attempt(readFile(target)), attempt(restart(sessionRoot, target)))
    finally deleteQuietly(directory)

  /** Kills the child once it has reported a varied number of finished saves; whatever it reported by its death counts.
    */
  private def killedAfter(round: Int, child: Process, errors: Path): Acks =
    val reader = new BufferedReader(new InputStreamReader(child.getInputStream, StandardCharsets.UTF_8))
    val target = 1 + (round * 5) % 9

    @tailrec def read(acks: Acks, killed: Boolean): Acks =
      Try(reader.readLine()).toOption.flatMap(Option(_)) match
        case None => acks
        case Some(line) =>
          val next = acks.merge(parsedAck(line))
          if !killed && next.lines >= target then
            child.destroyForcibly()
            read(next, killed = true)
          else read(next, killed)

    try
      val acks = read(Acks.none, killed = false)
      val _    = child.waitFor()
      val why  = Try(Files.readString(errors)).getOrElse("")
      withClue(s"round $round child died before reporting enough progress: $why")(acks.lines should be >= target)
      acks
    finally
      child.destroyForcibly()
      reader.close()

  final private case class Acks(fileAck: Int, sessionAck: Int, lines: Int):
    def merge(ack: Acks): Acks =
      Acks(fileAck max ack.fileAck, sessionAck max ack.sessionAck, lines + ack.lines)

  private object Acks:
    val none: Acks = Acks(0, 0, 0)

  private def parsedAck(line: String): Acks =
    line.split(' ') match
      case Array("file", n)    => Acks(n.toInt, 0, 1)
      case Array("session", n) => Acks(0, n.toInt, 1)
      case _                   => Acks.none

  private def childOf(sessionRoot: Path, target: Path, errors: Path): Process =
    val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    new ProcessBuilder(
      java,
      "-Djava.awt.headless=true",
      "-XX:TieredStopAtLevel=1",
      "-Xshare:auto",
      "-cp",
      testClasspath,
      "com.serenity.crash.KillRecoveryChild",
      sessionRoot.toString,
      target.toString
    ).redirectError(errors.toFile).start()

  // sbt runs these specs in its own JVM, whose `java.class.path` is not the project's; the build records the real one.
  private lazy val testClasspath: String =
    val stream = Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .getOrElse(fail("build.sbt did not generate crash/test-classpath.txt"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
    finally stream.close()

  private def readFile(target: Path): String =
    val loaded = new FileManager().loadFile(target, BufferId(0)).timeout(30.seconds).unsafeRunSync()
    loaded.document.content.collect()

  /** What startup does with the surviving session, then proof that the application can go on saving. */
  private def restart(sessionRoot: Path, target: Path): Startup =
    val sessions = SessionManager.create(
      sessionRoot,
      AppThemeManager.create,
      NoOpLogger[IO],
      SessionManager.SessionPolicy()
    )
    val files = new FileManager()
    val program =
      for
        setAside <- sessions.setAsideUnreadableCurrentSession()
        loaded   <- sessions.loadSession()
        offered <- loaded
          .flatMap(_.persisted.buffers.values.headOption)
          .traverse(restored =>
            files.loadFile(target, restored.id).map(disk => HotExitRecovery.offer(restored, disk).isDefined)
          )
        _      <- sessions.saveSession(loaded.getOrElse(AppState.initial))
        reload <- sessions.loadSession()
      yield Startup(setAside, loaded, offered.getOrElse(false), reload.isDefined)
    program.timeout(60.seconds).unsafeRunSync()

  private def attempt[A](body: => A): Either[Throwable, A] = Try(body).toEither

  private def deleteQuietly(directory: Path): Unit =
    Try {
      val stream = Files.walk(directory)
      try stream.sorted(Comparator.reverseOrder[Path]()).iterator.asScala.foreach(path => Try(Files.delete(path)))
      finally stream.close()
    }.getOrElse(())
