package com.serenity.state.models

/** A value derived from a source `S` (#1852). `inputs` reads everything `compute` depends on, and `references` names
  * those inputs: a [[Memo]] stays current while a later source still holds the very same references. State is only ever
  * replaced, never mutated, so an unchanged reference is an unchanged input, and the check costs one `eq` per input
  * however large the input is. A value-equal but rebuilt input counts as changed: that costs a recompute, never a stale
  * value.
  */
final case class DerivedValue[S, I, A](inputs: S => I, references: I => List[AnyRef], compute: I => A):

  /** `previous` itself while it is current for `source`; otherwise a memo of a fresh computation. */
  def refreshed(previous: Option[Memo[A]], source: S): Memo[A] =
    val read    = inputs(source)
    val current = references(read)
    previous.filter(_.isCurrentFor(current)).getOrElse(Memo(current, compute(read)))

  /** `memo`'s value while it is current for `source`; otherwise computed now, uncached. */
  def valueFor(memo: Option[Memo[A]], source: S): A =
    val read = inputs(source)
    memo.filter(_.isCurrentFor(references(read))).fold(compute(read))(_.value)

final case class Memo[A](inputs: List[AnyRef], value: A):

  def isCurrentFor(current: List[AnyRef]): Boolean =
    inputs.corresponds(current)(_ eq _)
