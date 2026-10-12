package com.serenity.app

import cats.effect.unsafe.implicits.global
import com.serenity.config.WaylandRender

/** Run by [[SkiaLazinessSpec]] in a separate JVM whose class loading the spec reads. Resolves the renderer exactly as
  * `Main` does for the launch kind named by the first argument, then reports the decision on stdout.
  */
object SkiaLazinessChild:

  def main(args: Array[String]): Unit =
    val launch = args.headOption match
      case Some("terminal") => GraphicsStartup.Launch(windowed = false, safeMode = false)
      case Some("safe")     => GraphicsStartup.Launch(windowed = true, safeMode = true)
      case Some("wayland-java2d") =>
        GraphicsStartup.Launch(windowed = true, safeMode = false, waylandRender = Some(WaylandRender.Java2D))
      case _ => GraphicsStartup.Launch(windowed = true, safeMode = false)
    val resolved = GraphicsStartup.current(launch).unsafeRunSync()
    System.out.println(s"resolved ${resolved.backend.logLine}")
    System.exit(0)
