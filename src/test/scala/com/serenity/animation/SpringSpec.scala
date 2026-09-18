package com.serenity.animation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[Spring]] is a per-tick physics integrator (issue #1082) rather than a closed-form `position(t)` function: every
  * other primitive in this package advances one discrete step per render tick, and a spring's velocity has to carry
  * over across a retarget mid-flight, which a time-parametric closed form cannot do without recomputing its whole
  * trajectory.
  */
class SpringSpec extends AnyFlatSpec with Matchers:

  private val Dt = 1.0 / 60.0

  "a spring at rest at its target" should "already be settled" in {
    Spring(position = 1.0, velocity = 0.0, target = 1.0).isSettled shouldBe true
  }

  it should "not advance once settled" in {
    val settled = Spring(position = 1.0, velocity = 0.0, target = 1.0)
    settled.advance(Dt) shouldBe settled
  }

  "a spring displaced from its target" should "move toward the target on each advance" in {
    val spring   = Spring(position = 0.0, velocity = 0.0, target = 1.0)
    val advanced = spring.advance(Dt)

    advanced.position should be > 0.0
    advanced.position should be < 1.0
    advanced.isSettled shouldBe false
  }

  it should "eventually settle at the target within a couple hundred milliseconds at 60fps" in {
    val ticksIn300ms = 18 // 300ms / (1/60s)
    val settled = (1 to ticksIn300ms).foldLeft(Spring(position = 0.0, velocity = 0.0, target = 1.0)) {
      case (spring, _) => spring.advance(Dt)
    }

    settled.isSettled shouldBe true
    settled.position shouldBe 1.0 +- 0.01
  }

  it should "never fully settle in fewer than a handful of ticks (it is not an instant snap)" in {
    val spring = Spring(position = 0.0, velocity = 0.0, target = 1.0)
    spring.advance(Dt).isSettled shouldBe false
    spring.advance(Dt).advance(Dt).isSettled shouldBe false
  }

  "retarget" should "change the target without resetting position or velocity" in {
    val spring    = Spring(position = 0.0, velocity = 0.0, target = 1.0).advance(Dt).advance(Dt)
    val midflight = spring.position
    val velocity  = spring.velocity

    val retargeted = spring.retarget(2.0)

    retargeted.position shouldBe midflight
    retargeted.velocity shouldBe velocity
    retargeted.target shouldBe 2.0
    retargeted.isSettled shouldBe false
  }

  it should "carry the mid-flight velocity into motion toward the new target rather than starting from rest" in {
    val movingTowardOne = Spring(position = 0.0, velocity = 0.0, target = 1.0).advance(Dt).advance(Dt).advance(Dt)
    val retargeted      = movingTowardOne.retarget(2.0)

    // The spring was already moving in the positive direction; retargeting further in the same direction should not
    // reset its velocity to zero, unlike a fixed-step tween restarting at its own `start`.
    retargeted.velocity shouldBe movingTowardOne.velocity
    retargeted.velocity should be > 0.0
  }

  "the default stiffness/damping" should "be critically damped (damping = 2*sqrt(stiffness) for unit mass)" in {
    Spring.DefaultDamping shouldBe (2.0 * math.sqrt(Spring.DefaultStiffness)) +- 1e-9
  }

  it should "settle a unit displacement in roughly 200-300ms at 60fps" in {
    val ticksAt200ms = (0.200 / Dt).round.toInt
    val ticksAt300ms = (0.300 / Dt).round.toInt

    val afterShortWindow = (1 to ticksAt200ms).foldLeft(Spring(position = 0.0, velocity = 0.0, target = 1.0)) {
      case (spring, _) => spring.advance(Dt)
    }
    // Not settled quite yet at 200ms -- otherwise the "roughly 200-300ms" claim would just as well be "under 200ms".
    afterShortWindow.position shouldBe 1.0 +- 0.1

    val afterFullWindow = (1 to ticksAt300ms).foldLeft(Spring(position = 0.0, velocity = 0.0, target = 1.0)) {
      case (spring, _) => spring.advance(Dt)
    }
    afterFullWindow.isSettled shouldBe true
  }
