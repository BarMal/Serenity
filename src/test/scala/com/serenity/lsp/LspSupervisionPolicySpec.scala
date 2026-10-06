package com.serenity.lsp

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspSupervisionPolicySpec extends AnyFlatSpec with Matchers:

  private val policy = LspSupervisionPolicy.Default

  "LspSupervisionPolicy" should "double the restart delay with each recent crash" in {
    policy.afterCrash(List(0.seconds), 0.seconds) shouldBe LspRestartDecision.RestartAfter(1.second)
    policy.afterCrash(List(10.seconds, 0.seconds), 10.seconds) shouldBe LspRestartDecision.RestartAfter(2.seconds)
    policy.afterCrash(List(20.seconds, 10.seconds, 0.seconds), 20.seconds) shouldBe
      LspRestartDecision.RestartAfter(4.seconds)
  }

  it should "give up once a server crashes more than three times within three minutes" in {
    val crashes = List(30.seconds, 20.seconds, 10.seconds, 0.seconds)

    policy.afterCrash(crashes, 30.seconds) shouldBe LspRestartDecision.GiveUp(4)
    policy.mayConnect(crashes, 2.minutes) shouldBe false
  }

  it should "forget crashes older than the restart window" in {
    val crashes = List(30.seconds, 20.seconds, 10.seconds, 0.seconds)

    policy.recentCrashes(crashes, 3.minutes + 15.seconds) shouldBe List(30.seconds, 20.seconds)
    policy.mayConnect(crashes, 3.minutes + 15.seconds) shouldBe true
  }

  it should "hold a new connection back until the last crash's backoff has passed" in {
    val crashes = List(10.seconds, 0.seconds)

    policy.mayConnect(crashes, 11.seconds) shouldBe false
    policy.mayConnect(crashes, 12.seconds) shouldBe true
    policy.mayConnect(Nil, 0.seconds) shouldBe true
  }
