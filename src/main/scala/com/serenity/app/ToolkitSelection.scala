package com.serenity.app

import java.util.Locale

import cats.effect.IO

/** Picks the AWT toolkit before anything loads AWT.
  *
  * A JetBrains Runtime can talk to Wayland directly (`WLToolkit`) instead of going through XWayland, but only if
  * `awt.toolkit.name` is set before the first `java.awt` class initialises: that is when libawt loads and fixes the
  * toolkit for the life of the process. A jpackage launcher cannot make its `--java-options` conditional on the
  * session, so the choice is made here, at the top of `Main.run`. Stock JDKs ignore the property.
  */
object ToolkitSelection:

  val OverrideVariable: String = "SERENITY_TOOLKIT"
  val ToolkitProperty: String  = "awt.toolkit.name"

  private val WaylandToolkitClassFile = "sun/awt/wl/WLToolkit.class"

  enum Choice(val toolkitProperty: Option[String]):
    case Wayland         extends Choice(Some("WLToolkit"))
    case X11             extends Choice(Some("XToolkit"))
    case PlatformDefault extends Choice(None)

  final case class Decision(choice: Choice, reason: String)

  final case class Environment(
      osName: String,
      properties: Map[String, String],
      env: Map[String, String],
      waylandToolkitAvailable: Boolean
  )

  def choose(environment: Environment): Decision =
    val requested = environment.env.get(OverrideVariable).map(_.trim.toLowerCase(Locale.ROOT)).filter(_.nonEmpty)
    if environment.properties.get(ToolkitProperty).exists(_.nonEmpty) then
      Decision(Choice.PlatformDefault, s"$ToolkitProperty was set on the command line")
    else if !environment.osName.toLowerCase(Locale.ROOT).contains("linux") then
      Decision(Choice.PlatformDefault, s"${environment.osName} has one toolkit")
    else
      requested match
        case Some("x11")         => Decision(Choice.X11, s"$OverrideVariable=x11")
        case Some("wayland")     => forcedWayland(environment)
        case Some("auto") | None => detected(environment, "")
        case Some(other)         => detected(environment, s" ($OverrideVariable=$other is not x11, wayland or auto)")

  private def forcedWayland(environment: Environment): Decision =
    if environment.waylandToolkitAvailable then Decision(Choice.Wayland, s"$OverrideVariable=wayland")
    else
      Decision(
        Choice.PlatformDefault,
        s"$OverrideVariable=wayland needs a JetBrains Runtime, but ${vendor(environment)} has no WLToolkit"
      )

  private def detected(environment: Environment, note: String): Decision =
    val waylandSession = environment.env.get("WAYLAND_DISPLAY").exists(_.trim.nonEmpty)
    if waylandSession && environment.waylandToolkitAvailable then
      Decision(Choice.Wayland, s"Wayland session on ${vendor(environment)}$note")
    else if waylandSession then
      Decision(Choice.PlatformDefault, s"Wayland session, but ${vendor(environment)} has no WLToolkit$note")
    else Decision(Choice.PlatformDefault, s"not a Wayland session$note")

  private def vendor(environment: Environment): String =
    environment.properties.get("java.vm.vendor").orElse(environment.properties.get("java.vendor")).getOrElse("this JVM")

  /** Reads the running JVM. The class file is looked up as a resource so that nothing in `java.awt` gets loaded. */
  def currentEnvironment: IO[Environment] =
    IO {
      val properties = List(ToolkitProperty, "java.vendor", "java.vm.vendor", "java.runtime.name").flatMap(key =>
        Option(System.getProperty(key)).map(key -> _)
      )
      Environment(
        osName = System.getProperty("os.name", ""),
        properties = properties.toMap,
        env = sys.env,
        waylandToolkitAvailable = Option(ClassLoader.getSystemResource(WaylandToolkitClassFile)).nonEmpty
      )
    }

  /** Must run before the first `java.awt` class is touched. */
  def install: IO[Decision] =
    currentEnvironment
      .map(choose)
      .flatTap(decision => IO(decision.choice.toolkitProperty.foreach(System.setProperty(ToolkitProperty, _))).void)
