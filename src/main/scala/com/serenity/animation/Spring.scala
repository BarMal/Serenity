package com.serenity.animation

/** A per-tick spring-physics integrator (issue #1082), advanced one discrete step per render tick the same way every
  * other primitive in this package is -- deliberately not a closed-form `position(t)`. A time-parametric spring can't
  * be retargeted mid-flight without recomputing its whole trajectory from a new `t=0`; this integrator just keeps
  * stepping the same unit-mass damped-harmonic-oscillator equation with a new `target`, so `retarget` is a single field
  * change and the existing `velocity` carries over untouched.
  *
  * Model (unit mass): `velocity += (stiffness * (target - position) - damping * velocity) * dt`, then
  * `position += velocity * dt`. `isSettled` substitutes for the fixed step count `ScalarTimeline`/`Tween` use to know
  * they're done -- a spring has no fixed duration, so completion is "close enough to the target and slow enough"
  * instead, per `epsilon`.
  *
  * The velocity half of that check is compared against `epsilon * sqrt(stiffness)`, not `epsilon` itself: velocity
  * carries units of (displacement / time), and a critically damped spring's peak velocity scales with its natural
  * frequency `sqrt(stiffness)`, so a fixed absolute threshold either barely constrains a soft spring or takes far
  * longer than the position criterion to satisfy on a stiff one (verified empirically while choosing `DefaultStiffness`
  * below -- a plain `epsilon` comparison on velocity pushed the settle time for this default well past a second).
  * Scaling by `sqrt(stiffness)` keeps both halves of the check on a comparable footing whatever `stiffness` a caller
  * picks, `Default` or their own.
  */
final case class Spring(
    position: Double,
    velocity: Double,
    target: Double,
    stiffness: Double = Spring.DefaultStiffness,
    damping: Double = Spring.DefaultDamping,
    epsilon: Double = Spring.DefaultEpsilon
):

  def isSettled: Boolean =
    math.abs(target - position) < epsilon && math.abs(velocity) < epsilon * math.sqrt(stiffness)

  def advance(dtSeconds: Double): Spring =
    if isSettled then this
    else
      val acceleration = stiffness * (target - position) - damping * velocity
      val nextVelocity = velocity + acceleration * dtSeconds
      val nextPosition = position + nextVelocity * dtSeconds
      copy(position = nextPosition, velocity = nextVelocity)

  /** Redirects the spring toward a new target, keeping its current position and velocity exactly as they are -- the
    * spring equation above only ever looks at `target - position`, so this is the entire retarget: no other field needs
    * to change for motion to continue smoothly from wherever the spring currently is.
    */
  def retarget(newTarget: Double): Spring = copy(target = newTarget)

object Spring:

  /** One render tick at 60fps -- the cadence every tick-driven primitive in this package assumes
    * (`AppRuntime.AnimationTickCadence`). Callers driving `advance` from a different tick rate should pass their own
    * `dtSeconds` instead of this constant.
    */
  val DefaultDt: Double = 1.0 / 60.0

  /** 1% of a unit displacement -- the position half of `isSettled`; see the class doc for the velocity half. */
  val DefaultEpsilon: Double = 0.01

  /** Chosen, together with `DefaultEpsilon`, so a unit displacement -- critically damped, integrated one discrete step
    * at a time at 60fps the way `advance` actually runs, not a continuous closed form -- settles in roughly 200-300ms:
    * fast enough to read as immediate, slow enough to still read as motion rather than a snap.
    *
    * Simulating `advance` at `DefaultDt` across a range of stiffness values (see this issue's PR description for the
    * script) shows the settle time falling inside that window from about 700 up to the point explicit-Euler integration
    * itself goes unstable at this tick rate (`damping = 2*sqrt(stiffness)` grows with `stiffness`, and `damping * dt`
    * approaching 2 makes the discrete velocity update overshoot rather than decay) -- instability sets in around
    * 2200-2400 here. 900 sits well inside the stable range with a comfortable margin below that ceiling, settling a
    * unit displacement in ~280ms (18 fewer ticks than the 300ms upper bound), and gives a damping of exactly 60 -- a
    * tidier constant than the 700/~52.9 pair a purely continuous derivation suggests, at no cost to the target feel.
    */
  val DefaultStiffness: Double = 900.0

  /** Critical damping for unit mass: `damping = 2*sqrt(stiffness)`. Below this the spring overshoots and oscillates;
    * above it, it slows down without ever overshooting but takes longer to settle. Critical damping is the fastest
    * approach to the target with no overshoot, which is the "settled" feel this default is chosen for.
    */
  val DefaultDamping: Double = 2.0 * math.sqrt(DefaultStiffness)
