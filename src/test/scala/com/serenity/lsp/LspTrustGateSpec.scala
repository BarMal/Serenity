package com.serenity.lsp

import java.nio.file.{Path, Paths}
import java.time.Instant

import scala.concurrent.duration.*

import cats.effect.std.Supervisor
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.lsp.client.LspMessageRequest
import com.serenity.lsp.config.{LanguageId, LspServerBinary, LspServerConfig}
import com.serenity.trust.{CommandSource, ProjectKey, ProjectTrust, ProjectTrustAccess}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The interim rule (#2526, owner decision (a)): a language server starts only when its workspace root is trusted;
  * otherwise it stays off and one notice per root offers to trust the folder.
  */
class LspTrustGateSpec extends AnyFlatSpec with Matchers:

  private val root: Path = Paths.get("/work/app").toAbsolutePath
  private val metals     = LspServerConfig(LanguageId.Scala, LspServerBinary.Metals)

  final private class FakeTrust(state: Ref[IO, ProjectTrust]) extends ProjectTrustAccess:
    override def current: IO[ProjectTrust]               = state.get
    override def trustFolder(root: ProjectKey): IO[Unit] = state.update(_.trust(root, Instant.EPOCH))
    override def revokeCovering(root: ProjectKey): IO[List[ProjectKey]] =
      state.modify(trust => (trust.covering(root).foldLeft(trust)(_.revoke(_)), trust.covering(root)))

  final private case class Harness(
      gate: LspTrustGate,
      trust: Ref[IO, ProjectTrust],
      questions: Ref[IO, List[(String, LspMessageRequest)]],
      trusted: Deferred[IO, ProjectKey],
      asked: Deferred[IO, Unit]
  )

  private def harness[A](initial: ProjectTrust, answer: Option[Int])(test: Harness => IO[A]): A =
    Supervisor[IO]
      .use { supervisor =>
        for
          trust     <- Ref.of[IO, ProjectTrust](initial)
          questions <- Ref.of[IO, List[(String, LspMessageRequest)]](Nil)
          trusted   <- Deferred[IO, ProjectKey]
          asked     <- Deferred[IO, Unit]
          ask = (server: String, request: LspMessageRequest) =>
            questions.update(_ :+ (server -> request)) >> asked.complete(()).void.as(answer)
          gate   <- LspTrustGate.create(FakeTrust(trust), ask, supervisor, key => trusted.complete(key).void)
          result <- test(Harness(gate, trust, questions, trusted, asked))
        yield result
      }
      .unsafeRunSync()

  "LspTrustGate" should "admit a server whose root is trusted, without asking" in {
    val trusted = ProjectTrust.empty.trust(ProjectKey.of(root), Instant.EPOCH)
    val (admitted, questions) =
      harness(trusted, answer = None)(h => (h.gate.admit(root, metals, CommandSource.BuiltIn), h.questions.get).tupled)

    admitted shouldBe true
    questions shouldBe empty
  }

  it should "admit a server whose root lies inside a trusted folder" in {
    val trusted = ProjectTrust.empty.trust(ProjectKey.of(root.getParent), Instant.EPOCH)

    harness(trusted, answer = None)(_.gate.admit(root, metals, CommandSource.BuiltIn)) shouldBe true
  }

  it should "keep the server off for an untrusted root and offer, once, to trust the folder" in {
    val (first, second, questions) = harness(ProjectTrust.empty, answer = None) { h =>
      for
        first     <- h.gate.admit(root, metals, CommandSource.BuiltIn)
        _         <- h.asked.get.timeout(5.seconds)
        second    <- h.gate.admit(root, metals, CommandSource.BuiltIn)
        questions <- h.questions.get
      yield (first, second, questions)
    }

    first shouldBe false
    second shouldBe false
    questions.map(_._1) shouldBe List("metals")
    questions.flatMap(_._2.actions.map(_.title)) shouldBe List(LspTrustGate.TrustAction)
    questions.head._2.text should include(root.toString)
  }

  it should "trust the folder, and say so, when the user accepts" in {
    val (key, nowAdmitted) = harness(ProjectTrust.empty, answer = Some(0)) { h =>
      for
        _           <- h.gate.admit(root, metals, CommandSource.BuiltIn)
        key         <- h.trusted.get.timeout(5.seconds)
        nowAdmitted <- h.gate.admit(root, metals, CommandSource.BuiltIn)
      yield (key, nowAdmitted)
    }

    key shouldBe ProjectKey.of(root)
    nowAdmitted shouldBe true
  }

  it should "leave the folder untrusted when the notice is dismissed" in {
    val (stillTrusted, signalled) = harness(ProjectTrust.empty, answer = None) { h =>
      for
        _         <- h.gate.admit(root, metals, CommandSource.BuiltIn)
        _         <- h.asked.get.timeout(5.seconds)
        trust     <- h.trust.get
        signalled <- h.trusted.tryGet
      yield (trust.isTrusted(ProjectKey.of(root)), signalled)
    }

    stillTrusted shouldBe false
    signalled shouldBe None
  }

  it should "trust a folder on request, as accepting the notice does" in {
    val (key, admitted) = harness(ProjectTrust.empty, answer = None) { h =>
      for
        key      <- h.gate.trustFolder(root)
        signal   <- h.trusted.get.timeout(5.seconds)
        admitted <- h.gate.admit(root, metals, CommandSource.BuiltIn)
      yield (signal == key, admitted)
    }

    key shouldBe true
    admitted shouldBe true
  }

  it should "revoke the folders covering a root, so its servers are refused again" in {
    val trusted = ProjectTrust.empty.trust(ProjectKey.of(root.getParent), Instant.EPOCH)
    val (revoked, admitted) = harness(trusted, answer = None) { h =>
      (h.gate.revokeCovering(root), h.gate.admit(root, metals, CommandSource.BuiltIn)).tupled
    }

    revoked shouldBe List(ProjectKey.of(root.getParent))
    admitted shouldBe false
  }
