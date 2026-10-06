package com.serenity.session

import java.lang.management.ManagementFactory

import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import com.serenity.testkit.ConfigGenerators
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The session file is what a build reads back after the next release, so remembering the config's JSON must not change
  * a byte of it.
  */
class SessionStateEncoderSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance = Balance.default

  private def stateWith(config: com.serenity.config.AppConfig): SessionState =
    SessionState
      .snapshot(AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = config)), true)
      .state

  "SessionStateEncoder" should "write the bytes the plain encoder writes, however often and in whatever order configs come" in
    forAll(ConfigGenerators.genAppConfig, ConfigGenerators.genAppConfig) { (first, second) =>
      val encoder = new SessionStateEncoder
      List(first, second, first, first, second).foreach { config =>
        val state = stateWith(config)
        encoder.compact(state) shouldBe SessionWriteJournal.compact[SessionState](state)
      }
    }

  it should "encode a config equal to the last one without building its JSON again" in {
    val memory = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    assume(memory.isThreadAllocatedMemorySupported)
    def allocated() = memory.getThreadAllocatedBytes(Thread.currentThread.threadId)
    val encoder     = new SessionStateEncoder
    val state       = stateWith(com.serenity.config.AppConfig.default)
    (1 to 20).foreach(_ => encoder.compact(state))
    val before = allocated()
    (1 to 20).foreach(_ => encoder.compact(state))
    val perEncode = (allocated() - before) / 20
    info(f"[ALLOC-BUDGET] SessionStateEncoder.compact with an unchanged config: $perEncode%,d B")
    perEncode should be < 112L * 1024
  }
