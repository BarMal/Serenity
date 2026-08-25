package com.serenity.ui.renderer

import cats.syntax.semigroup.*
import com.serenity.state.models.Damage

/** Pure accumulation logic standing in for `Renderer`'s current `WeakHashMap[AnyRef, FrameRecord]`-based structural
  * diff (`frameRecords`/`publishedRecords`), once `#999` finishes wiring `planFrame` to consume `Damage` instead of
  * reconstructing and comparing a full frame description every frame. Landed here, tested standalone; `planFrame` does
  * not read this yet -- see `DamageProducer`'s doc comment for what still has to land before it safely can.
  *
  * Two independent accumulators mirror the two independent comparisons `planFrame` makes today:
  *
  *   - '''Per buffer''' (`accumulateBuffers`/`observeBufferDraw`): "what changed since this specific pixel buffer was
  *     last drawn into." Necessary because `SwingWindow.ReusableImagePool` (`SwingWindow.scala:650-665`) alternates
  *     between two images, so a buffer's own damage history spans however many frames it has been sitting unused, not
  *     just the one frame immediately prior -- accumulating via `Damage`'s `Monoid` across that whole span is exactly
  *     what a `Map[AnyRef, Damage]` keyed by buffer identity gives for free.
  *   - '''Screen''' (`accumulateScreen`/`observeScreenPublish`): "what changed since the currently-displayed image was
  *     published." Exactly one buffer is ever on screen at a time, so this needs only one `Damage` value, reset each
  *     time a frame is actually published rather than per buffer identity.
  *
  * Every operation here is a pure function over immutable values so the accumulation semantics can be proven correct in
  * isolation; the mutable, synchronized shell `frameRecords`/`publishedRecords` currently provide is a wiring concern
  * for whichever later change actually calls these from `Renderer`.
  */
object DamageAccumulator:

  /** Folds `damage` into every buffer identity already being tracked, via `Damage`'s `Monoid`. An identity not yet
    * tracked is left absent -- only [[observeBufferDraw]] starts tracking one, at the point it is first drawn into, so
    * accumulation before that point has nothing to attach to.
    */
  def accumulateBuffers(tracked: Map[AnyRef, Damage], damage: Damage): Map[AnyRef, Damage] =
    if damage == Damage.Nothing then tracked
    else tracked.view.mapValues(_ |+| damage).toMap

  /** The damage accumulated for `identity` since it was last drawn into (`Damage.Nothing` if never seen before), and
    * the updated tracking map with `identity` reset to `Damage.Nothing` -- and, if this is the first time `identity`
    * appears, now present so future [[accumulateBuffers]] calls reach it.
    */
  def observeBufferDraw(tracked: Map[AnyRef, Damage], identity: AnyRef): (Damage, Map[AnyRef, Damage]) =
    val damage = tracked.getOrElse(identity, Damage.Nothing)
    (damage, tracked.updated(identity, Damage.Nothing))

  /** Folds `damage` into the screen's accumulated damage since its last publish. */
  def accumulateScreen(current: Damage, damage: Damage): Damage =
    current |+| damage

  /** The damage accumulated since the screen was last published, and the reset (`Damage.Nothing`) value to store going
    * forward.
    */
  def observeScreenPublish(current: Damage): (Damage, Damage) =
    (current, Damage.Nothing)
