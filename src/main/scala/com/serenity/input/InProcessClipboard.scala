package com.serenity.input

import cats.effect.{Ref, Sync}
import cats.syntax.all.*

/** The last-resort clipboard strategy: copy/paste works within Serenity only, scoped to this run. */
object InProcessClipboard:

  def apply[F[_] : Sync]: F[SystemClipboard[F]] =
    Ref.of[F, Option[String]](None).map { ref =>
      SystemClipboard(readText = ref.get, writeText = text => ref.set(Some(text)))
    }
