package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.config.SpellCheckConfig
import com.serenity.spellcheck.DictionarySnapshot

/** The dictionary last loaded, kept until the spell-check config changes or its files are reported changed, so an
  * analysis pass over unchanged dictionaries does no filesystem work.
  */
final private[manager] class HeldDictionary private (
    held: Ref[IO, HeldDictionary.Held],
    load: SpellCheckConfig => DictionarySnapshot
):

  /** The held dictionary for `config`, otherwise a fresh load. A load that began before a [[discard]] is not kept,
    * because it may have read the files before they changed.
    */
  def snapshotFor(config: SpellCheckConfig): IO[DictionarySnapshot] =
    val normalized = config.normalized
    held.get.flatMap { current =>
      current.loaded.collect { case (heldConfig, snapshot) if heldConfig == normalized => snapshot } match
        case Some(snapshot) => IO.pure(snapshot)
        case None =>
          detachedBlocking(load(config)).flatTap(snapshot =>
            held.update(now =>
              if now.generation == current.generation then now.copy(loaded = Some(normalized -> snapshot)) else now
            )
          )
    }

  /** A parse cannot be interrupted, and cancelling a lane job waits for it to settle, so quitting or a newer analysis
    * would wait out the rest of a dictionary parse (seconds on a loaded machine). The load runs on its own fiber and
    * only the wait for it is cancelled; the shared cache keeps what it finishes for the next analysis.
    */
  private def detachedBlocking[A](work: => A): IO[A] = IO.blocking(work).start.flatMap(_.joinWithNever)

  def discard: IO[Unit] = held.update(now => HeldDictionary.Held(now.generation + 1, None))

private[manager] object HeldDictionary:

  final case class Held(generation: Long, loaded: Option[(SpellCheckConfig, DictionarySnapshot)])

  def create(load: SpellCheckConfig => DictionarySnapshot): IO[HeldDictionary] =
    Ref.of[IO, Held](Held(0L, None)).map(new HeldDictionary(_, load))
