package com.serenity.app

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.SocketChannel
import java.nio.file.Path

import scala.util.control.NonFatal

/** Whether the Wayland compositor a session names takes a connection, found the way libwayland's `wl_display_connect`
  * finds it. A session can name a socket nothing listens on (a compositor that exited, an environment inherited through
  * `su` or `ssh`), and the JetBrains Runtime's `WLToolkit` then has no fallback: it ends up headless.
  */
object WaylandSocket:

  private val DefaultName = "wayland-0"

  /** Why the socket cannot be used, or `None` when it connects (or cannot be probed without using it up). */
  def problem(env: Map[String, String]): Option[String] =
    if present(env, "WAYLAND_SOCKET") then None
    else
      val name = env.get("WAYLAND_DISPLAY").map(_.trim).filter(_.nonEmpty).getOrElse(DefaultName)
      socketPath(env, name).fold(Some(_), connect)

  private def present(env: Map[String, String], key: String): Boolean =
    env.get(key).exists(_.trim.nonEmpty)

  private def socketPath(env: Map[String, String], name: String): Either[String, Path] =
    if name.startsWith("/") then Right(Path.of(name))
    else
      env
        .get("XDG_RUNTIME_DIR")
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(dir => Path.of(dir).resolve(name))
        .toRight(s"WAYLAND_DISPLAY=$name is not an absolute path and XDG_RUNTIME_DIR is not set")

  private def connect(socket: Path): Option[String] =
    try
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      try
        val _ = channel.connect(UnixDomainSocketAddress.of(socket))
        None
      finally channel.close()
    catch
      case NonFatal(failure) =>
        Some(s"$socket: ${Option(failure.getMessage).getOrElse(failure.getClass.getSimpleName)}")
