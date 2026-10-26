package com.serenity

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.ServerSocketChannel
import java.nio.file.{Files, Path}

import com.serenity.app.WaylandSocket
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Whether a Wayland session's socket takes a connection, probed the way libwayland's `wl_display_connect` finds it.
  * The sockets are real ones under a scratch folder; the paths stay short, as a Unix socket path is limited to about a
  * hundred bytes.
  */
class WaylandSocketSpec extends AnyFlatSpec with Matchers:

  private def serving[A](use: (Path, Path) => A): A =
    assume(
      !System.getProperty("os.name", "").toLowerCase.contains("windows"),
      "Unix sockets are a Linux and macOS matter"
    )
    TestTemp.within("w") { runtime =>
      val socket = runtime.resolve("wl-1")
      val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
      try
        server.bind(UnixDomainSocketAddress.of(socket))
        use(runtime, socket)
      finally server.close()
    }

  "WaylandSocket.problem" should "find nothing wrong with a socket that is listening, named relative to XDG_RUNTIME_DIR" in
    serving { (runtime, _) =>
      WaylandSocket.problem(Map("XDG_RUNTIME_DIR" -> runtime.toString, "WAYLAND_DISPLAY" -> "wl-1")) shouldBe None
    }

  it should "find nothing wrong with a listening socket named by an absolute path" in
    serving((_, socket) => WaylandSocket.problem(Map("WAYLAND_DISPLAY" -> socket.toString)) shouldBe None)

  it should "report the socket file a compositor left behind when it exited" in {
    assume(
      !System.getProperty("os.name", "").toLowerCase.contains("windows"),
      "Unix sockets are a Linux and macOS matter"
    )
    TestTemp.within("w") { runtime =>
      val socket = runtime.resolve("wl-1")
      ServerSocketChannel.open(StandardProtocolFamily.UNIX).bind(UnixDomainSocketAddress.of(socket)).close()
      Files.exists(socket) shouldBe true
      val problem = WaylandSocket.problem(Map("XDG_RUNTIME_DIR" -> runtime.toString, "WAYLAND_DISPLAY" -> "wl-1"))
      problem.getOrElse(fail("a socket with no listener connected")) should include(socket.toString)
    }
  }

  it should "report a socket that does not exist" in
    TestTemp.within("w") { runtime =>
      val problem = WaylandSocket.problem(Map("XDG_RUNTIME_DIR" -> runtime.toString, "WAYLAND_DISPLAY" -> "wl-9"))
      problem.getOrElse(fail("a missing socket connected")) should include("wl-9")
    }

  it should "try wayland-0 when WAYLAND_DISPLAY is not set, as libwayland does" in
    TestTemp.within("w") { runtime =>
      val problem = WaylandSocket.problem(Map("XDG_RUNTIME_DIR" -> runtime.toString))
      problem.getOrElse(fail("a missing socket connected")) should include("wayland-0")
    }

  it should "report a relative name when XDG_RUNTIME_DIR is not set, since libwayland refuses it too" in {
    val problem = WaylandSocket.problem(Map("WAYLAND_DISPLAY" -> "wayland-1"))
    problem.getOrElse(fail("a socket was found with nowhere to look")) should include("XDG_RUNTIME_DIR")
  }

  it should "trust WAYLAND_SOCKET, a connection already open that a probe could only use up" in {
    WaylandSocket.problem(Map("WAYLAND_SOCKET" -> "7", "WAYLAND_DISPLAY" -> "wl-9")) shouldBe None
  }

  it should "report a path too long for a Unix socket instead of throwing" in {
    val problem = WaylandSocket.problem(Map("WAYLAND_DISPLAY" -> ("/" + "a" * 300)))
    problem should not be empty
  }
