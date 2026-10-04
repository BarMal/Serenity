package com.serenity.spike

import java.nio.file.{Path, Paths}

import cats.effect.unsafe.implicits.global
import com.serenity.app.ToolkitSelection

enum Pipeline:
  /** Draw the scene inside `SkiaLayer`'s render callback, on the EDT. */
  case Direct

  /** Record an `SkPicture` on a worker thread; the render callback replays it on the EDT. */
  case Picture

  /** Rasterise into a persistent offscreen surface on a worker thread (damaged region only); the callback blits it. */
  case Raster

final case class SpikeOptions(
    mode: String = "headless",
    pipeline: Pipeline = Pipeline.Direct,
    renderApi: Option[String] = None,
    document: Option[Path] = None,
    scale: Double = 2.0,
    warmupFrames: Int = 30,
    frames: Int = 120,
    keys: Int = 200,
    paceMs: Long = 60,
    idleSeconds: Int = 10,
    shapingIterations: Int = 20,
    screenshot: Option[Path] = None,
    shaping: Shaping = Shaping.Explicit,
    subpixelText: Boolean = true,
    fontFile: String = "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf",
    windowSize: (Int, Int) = (Geometry.LogicalWidth, Geometry.LogicalHeight),
    textCache: TextCache = TextCache.Rows,
    scrollBlit: Boolean = false,
    wheelRows: Int = 3,
    scrolls: Int = 120,
    vsync: Option[Boolean] = None
)

object SpikeOptions:
  val Usage: String =
    """usage: SkikoSpike [--mode=headless|shaping|window|interactive] [--pipeline=direct|picture|raster]
      |                  [--render-api=SOFTWARE_FAST|SOFTWARE_COMPAT|OPENGL|VULKAN] [--doc=lorem.txt] [--scale=2]
      |                  [--frames=120] [--keys=200] [--pace-ms=60] [--idle-s=10] [--iterations=20]
      |                  [--screenshot=out.png] [--shaper=explicit|reused|textline] [--subpixel-text=true|false]
      |                  [--font-file=/path/DejaVuSerif.ttf] [--window=1500x1000]
      |                  [--cache=rows|blob|picture|image] [--scroll-blit=true|false] [--wheel-rows=3] [--scrolls=120]
      |                  [--vsync=true|false]""".stripMargin

  def parse(args: List[String]): Either[String, SpikeOptions] =
    args.foldLeft[Either[String, SpikeOptions]](Right(SpikeOptions())) { (parsed, arg) =>
      parsed.flatMap { options =>
        arg.stripPrefix("--").split("=", 2).toList match
          case "mode" :: value :: Nil => Right(options.copy(mode = value))
          case "pipeline" :: value :: Nil =>
            Pipeline.values.find(_.toString.equalsIgnoreCase(value)).toRight(s"bad $arg").map(p =>
              options.copy(pipeline = p)
            )
          case "render-api" :: value :: Nil => Right(options.copy(renderApi = Some(value)))
          case "doc" :: value :: Nil        => Right(options.copy(document = Some(Paths.get(value))))
          case "scale" :: value :: Nil      => value.toDoubleOption.toRight(s"bad $arg").map(v => options.copy(scale = v))
          case "frames" :: value :: Nil     => value.toIntOption.toRight(s"bad $arg").map(v => options.copy(frames = v))
          case "keys" :: value :: Nil       => value.toIntOption.toRight(s"bad $arg").map(v => options.copy(keys = v))
          case "pace-ms" :: value :: Nil    => value.toLongOption.toRight(s"bad $arg").map(v => options.copy(paceMs = v))
          case "idle-s" :: value :: Nil     => value.toIntOption.toRight(s"bad $arg").map(v => options.copy(idleSeconds = v))
          case "iterations" :: value :: Nil =>
            value.toIntOption.toRight(s"bad $arg").map(v => options.copy(shapingIterations = v))
          case "screenshot" :: value :: Nil => Right(options.copy(screenshot = Some(Paths.get(value))))
          case "font-file" :: value :: Nil  => Right(options.copy(fontFile = value))
          case "window" :: value :: Nil =>
            value.split("x").toList.flatMap(_.toIntOption) match
              case w :: h :: Nil => Right(options.copy(windowSize = (w, h)))
              case _             => Left(s"bad $arg")
          case "subpixel-text" :: value :: Nil => value.toBooleanOption.toRight(s"bad $arg").map(v => options.copy(subpixelText = v))
          case "cache" :: value :: Nil =>
            TextCache.values.find(_.toString.equalsIgnoreCase(value)).toRight(s"bad $arg").map(c =>
              options.copy(textCache = c)
            )
          case "scroll-blit" :: value :: Nil =>
            value.toBooleanOption.toRight(s"bad $arg").map(v => options.copy(scrollBlit = v))
          case "wheel-rows" :: value :: Nil => value.toIntOption.toRight(s"bad $arg").map(v => options.copy(wheelRows = v))
          case "scrolls" :: value :: Nil    => value.toIntOption.toRight(s"bad $arg").map(v => options.copy(scrolls = v))
          case "vsync" :: value :: Nil =>
            value.toBooleanOption.toRight(s"bad $arg").map(v => options.copy(vsync = Some(v)))
          case "shaper" :: "explicit" :: Nil => Right(options.copy(shaping = Shaping.Explicit))
          case "shaper" :: "reused" :: Nil   => Right(options.copy(shaping = Shaping.Reused))
          case "shaper" :: "textline" :: Nil => Right(options.copy(shaping = Shaping.TextLineMake))
          case _                            => Left(s"unknown argument $arg")
      }
    }

/** Throwaway Skiko spike for #1812's go/no-go numbers. Not part of the app; see `spike/README.md`. */
object SkikoSpike:

  def main(args: Array[String]): Unit =
    SpikeOptions.parse(args.toList) match
      case Left(message) =>
        Console.err.println(s"$message\n${SpikeOptions.Usage}")
        sys.exit(2)
      case Right(options) =>
        // Must precede the first java.awt class load, exactly as Main does.
        val toolkit = ToolkitSelection.install.unsafeRunSync()
        Report.note("env.toolkit", s"${toolkit.choice} (${toolkit.reason})")
        options.renderApi.foreach(api => System.setProperty("skiko.renderApi", api))
        options.vsync.foreach(on => System.setProperty("skiko.vsync.enabled", on.toString))
        Report.note(
          "env.jvm",
          s"${System.getProperty("java.vm.vendor")} ${System.getProperty("java.version")} " +
            s"cores=${Runtime.getRuntime.availableProcessors} SKIKO_RENDER_API=${sys.env.getOrElse("SKIKO_RENDER_API", "")} " +
            s"skiko.renderApi=${Option(System.getProperty("skiko.renderApi")).getOrElse("")} " +
            s"skiko.vsync.enabled=${Option(System.getProperty("skiko.vsync.enabled")).getOrElse("")} " +
            s"skiko.gpu.resourceCacheLimit=${Option(System.getProperty("skiko.gpu.resourceCacheLimit")).getOrElse("")}"
        )
        options.mode match
          case "headless"    => HeadlessBench.run(options)
          case "shaping"     => ShapingBench.run(options)
          case "window"      => WindowBench.run(options, interactive = false)
          case "interactive" => WindowBench.run(options, interactive = true)
          case other =>
            Console.err.println(s"unknown mode $other\n${SpikeOptions.Usage}")
            sys.exit(2)
        if options.mode != "interactive" then sys.exit(0)
