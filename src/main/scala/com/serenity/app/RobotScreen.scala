package com.serenity.app

import java.awt.{Rectangle, Robot}

import cats.effect.IO
import com.serenity.app.PixelClock.{Region, Screen}

/** Reads the screen with `java.awt.Robot`: what the toolkit exposes, not necessarily what the display scans out. It
  * needs a display (X11 or XWayland on Linux) and fails to start on a headless JVM.
  */
object RobotScreen:

  def apply(): IO[Screen] =
    IO.blocking(new Robot()).map { robot =>
      new Screen:
        def grab(region: Region): IO[Vector[Int]] =
          IO.blocking {
            val image  = robot.createScreenCapture(new Rectangle(region.x, region.y, region.width, region.height))
            val pixels = new Array[Int](region.width * region.height)
            image.getRGB(0, 0, region.width, region.height, pixels, 0, region.width).toVector
          }
    }
