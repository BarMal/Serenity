package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerEphemeralSessionRootSpec extends AnyFlatSpec with Matchers:

  "StateManager.ephemeralSessionRoot" should "make each session folder inside the root the test build owns" in
    TestTemp.within("ephemeral-root") { root =>
      val first  = StateManager.ephemeralSessionRoot(Some(root)).unsafeRunSync()
      val second = StateManager.ephemeralSessionRoot(Some(root)).unsafeRunSync()

      first.map(_.getParent) shouldBe Some(root)
      second.map(_.getParent) shouldBe Some(root)
      first should not be second
      first.map(Files.isDirectory(_)) shouldBe Some(true)
    }

  it should "make nothing when no test root is set, which is every real launch" in {
    StateManager.ephemeralSessionRoot(None).unsafeRunSync() shouldBe None
  }
