package com.serenity.input

import cats.effect.{Ref, Sync}
import cats.syntax.all.*

/** The last-resort clipboard strategy: copy/paste works within Serenity only, scoped to this run. */
object InProcessClipboard:

  def apply[F[_] : Sync]: F[SystemClipboard[F]] =
    Ref.of[F, Option[String]](None).map { ref =>
      new SystemClipboard[F]:
        override def readText: F[Option[String]]      = ref.get
        override def writeText(text: String): F[Unit] = ref.set(Some(text))
    }
