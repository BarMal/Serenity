package com.serenity.input

import java.nio.charset.StandardCharsets

import cats.effect.IO

/** A [[SystemClipboard]] backed by an external CLI tool (`wl-copy`/`wl-paste`, `xclip`, `xsel`) for a display-less,
  * terminal-writer-less environment.
  */
object ExternalToolClipboard:

  /** Runs one clipboard command to completion, feeding `stdin` (if any) and returning captured stdout. Abstracted from
    * the real process so specs can stub it instead of shelling out.
    */
  private[input] trait ProcessRunner:
    def run(command: List[String], stdin: Option[String]): IO[String]

  private object SystemProcessRunner extends ProcessRunner:

    override def run(command: List[String], stdin: Option[String]): IO[String] =
      IO.blocking {
        val builder = new ProcessBuilder(command*)
        builder.redirectErrorStream(false)
        val process = builder.start()
        stdin.foreach { text =>
          process.getOutputStream.write(text.getBytes(StandardCharsets.UTF_8))
          process.getOutputStream.close()
        }
        val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        process.waitFor()
        output
      }

  def apply(tool: ExternalClipboardTool): SystemClipboard[IO] = apply(tool, SystemProcessRunner)

  private[input] def apply(tool: ExternalClipboardTool, runner: ProcessRunner): SystemClipboard[IO] =
    new SystemClipboard[IO]:
      override def readText: IO[Option[String]] =
        runner.run(tool.readCommand, None).map(Option(_)).handleError(_ => None)

      override def writeText(text: String): IO[Unit] =
        runner.run(tool.writeCommand, Some(text)).void.handleError(_ => ())
