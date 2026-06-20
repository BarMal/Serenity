package com.serenity.io

import java.nio.file.Path

import cats.effect.IO

trait FileDialog:
  def chooseOpenFile(initialDirectory: Option[Path]): IO[Option[Path]]

  def chooseSaveFile(initialDirectory: Option[Path], suggestedFileName: Option[String]): IO[Option[Path]]

object FileDialog:

  val unavailable: FileDialog = new FileDialog:
    override def chooseOpenFile(initialDirectory: Option[Path]): IO[Option[Path]] =
      IO.pure(None)

    override def chooseSaveFile(initialDirectory: Option[Path], suggestedFileName: Option[String]): IO[Option[Path]] =
      IO.pure(None)
