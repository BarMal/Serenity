package com.serenity.lsp.config

import cats.effect.IO
import java.nio.file.{Files, Path, Paths}

object WorkspaceRootDetector:

  def detect(filePath: String, languageId: LanguageId): IO[Option[Path]] =
    IO.blocking {
      val start = Paths.get(filePath).getParent
      if start == null then None
      else walkUp(start, languageId)
    }

  private def walkUp(dir: Path, languageId: LanguageId): Option[Path] =
    val markers = RootMarker.forLanguage(languageId)
    if markers.exists(m => Files.exists(dir.resolve(m.filename))) then Some(dir)
    else if isGitRoot(dir) then Some(dir)
    else
      val parent = dir.getParent
      if parent == null || parent == dir then None
      else walkUp(parent, languageId)

  private def isGitRoot(dir: Path): Boolean =
    Files.exists(dir.resolve(".git"))
