package com.serenity.lsp

import cats.effect.IO
import fs2.Stream
import org.typelevel.log4cats.Logger

object LspManager:

  def run(effects: Stream[IO, LspEffect], logger: Logger[IO]): IO[Unit] =
    effects
      .evalMap {
        case LspEffect.FileOpened(uri, languageId, _) =>
          logger.info(s"[LSP] FileOpened lang=${languageId.id} uri=$uri")
        case LspEffect.FileChanged(uri, languageId, _, version) =>
          logger.debug(s"[LSP] FileChanged lang=${languageId.id} version=$version uri=$uri")
        case LspEffect.FileClosed(uri, languageId) =>
          logger.info(s"[LSP] FileClosed lang=${languageId.id} uri=$uri")
      }
      .compile
      .drain
