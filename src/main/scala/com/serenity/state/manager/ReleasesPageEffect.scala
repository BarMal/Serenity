package com.serenity.state.manager

import java.net.URI

import cats.effect.IO
import com.serenity.io.ReleasesPage
import com.serenity.state.models.{Notice, NoticeLevel}

private[manager] object ReleasesPageEffect:

  /** When no browser opens, the notice carries the URL so the user can still reach the page. */
  def open(browse: URI => IO[Unit], showNotice: Notice => IO[Unit]): IO[Unit] =
    browse(ReleasesPage.uri).handleErrorWith(_ =>
      showNotice(Notice(NoticeLevel.Warning, s"Couldn't open a browser. Releases are at ${ReleasesPage.url}"))
    )
