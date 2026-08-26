package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileDialog
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

trait StateManagerTestSupport:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given Balance           = Balance.default

  protected def testLogger(name: String): Logger[IO] =
    LoggerFactory[IO].getLogger(using LoggerName(name))

  protected def createStateManagerIO(
    loggerName: String,
    onFontConfigChanged: FontConfig => IO[Unit] = _ => IO.unit,
    deviceTextScaleProvider: IO[Double] = IO.pure(1.0),
    fileDialog: Option[FileDialog] = None
  ): IO[StateManager] =
    IO.blocking(Files.createTempDirectory(s"${loggerName.toLowerCase}-state-manager"))
      .flatMap(root =>
        StateManager.apply(
          testLogger(loggerName),
          onFontConfigChanged = onFontConfigChanged,
          deviceTextScaleProvider = deviceTextScaleProvider,
          sessionRootOverride = Some(root),
          fileDialog = fileDialog
        )
      )

  protected def createStateManager(
    loggerName: String,
    onFontConfigChanged: FontConfig => IO[Unit] = _ => IO.unit,
    deviceTextScaleProvider: IO[Double] = IO.pure(1.0),
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    createStateManagerIO(loggerName, onFontConfigChanged, deviceTextScaleProvider, fileDialog).unsafeRunSync()
