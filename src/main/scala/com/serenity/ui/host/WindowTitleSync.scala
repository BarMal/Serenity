package com.serenity.ui.host

import cats.effect.{IO, Ref}
import com.serenity.state.models.AppState

/** Keeps the window's title at [[WindowTitle]] of the state, touching the window only when the title changes rather
  * than on every frame.
  */
final class WindowTitleSync private (lastShown: Ref[IO, Option[String]], show: String => IO[Unit]):

  def sync(state: AppState): IO[Unit] =
    val title = WindowTitle.from(state)
    lastShown.modify(previous => (Some(title), !previous.contains(title))).flatMap(IO.whenA(_)(show(title)))

object WindowTitleSync:

  def create(show: String => IO[Unit]): IO[WindowTitleSync] =
    Ref.of[IO, Option[String]](None).map(new WindowTitleSync(_, show))
