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
      waylandToolkitAvailable: Boolean,
      waylandSocketProblem: Option[String]
  )

  def choose(environment: Environment): Decision =
    val requested = environment.env.get(OverrideVariable).map(_.trim.toLowerCase(Locale.ROOT)).filter(_.nonEmpty)
    if environment.properties.get(ToolkitProperty).exists(_.nonEmpty) then
      Decision(Choice.PlatformDefault, s"$ToolkitProperty was set on the command line")
    else if !isLinux(environment.osName) then Decision(Choice.PlatformDefault, s"${environment.osName} has one toolkit")
    else
      requested match
        case Some("x11")         => Decision(Choice.X11, s"$OverrideVariable=x11")
        case Some("wayland")     => forcedWayland(environment)
        case Some("auto") | None => detected(environment, "")
        case Some(other)         => detected(environment, s" ($OverrideVariable=$other is not x11, wayland or auto)")

  private def forcedWayland(environment: Environment): Decision =
    if !environment.waylandToolkitAvailable then
      Decision(
        Choice.PlatformDefault,
        s"$OverrideVariable=wayland needs a JetBrains Runtime, but ${vendor(environment)} has no WLToolkit"
      )
    else
      environment.waylandSocketProblem match
        case Some(problem) =>
          Decision(
            Choice.PlatformDefault,
            s"$OverrideVariable=wayland, but the Wayland socket does not connect ($problem)"
          )
        case None => Decision(Choice.Wayland, s"$OverrideVariable=wayland")

  private def detected(environment: Environment, note: String): Decision =
    val waylandSession = environment.env.get("WAYLAND_DISPLAY").exists(_.trim.nonEmpty)
    if waylandSession && environment.waylandToolkitAvailable then
      environment.waylandSocketProblem match
        case Some(problem) =>
          Decision(Choice.PlatformDefault, s"WAYLAND_DISPLAY is set, but the socket does not connect ($problem)$note")
        case None => Decision(Choice.Wayland, s"Wayland session on ${vendor(environment)}$note")
    else if waylandSession then
      Decision(Choice.PlatformDefault, s"Wayland session, but ${vendor(environment)} has no WLToolkit$note")
    else Decision(Choice.PlatformDefault, s"not a Wayland session$note")

  private def isLinux(osName: String): Boolean = osName.toLowerCase(Locale.ROOT).contains("linux")

  private def vendor(environment: Environment): String =
    environment.properties.get("java.vm.vendor").orElse(environment.properties.get("java.vendor")).getOrElse("this JVM")

  /** Reads the running JVM. The class file is looked up as a resource so that nothing in `java.awt` gets loaded. The
    * socket is probed only where its answer can matter: a Linux runtime with `WLToolkit`, in a session that names a
    * Wayland socket or is asked for one.
    */
  def currentEnvironment: IO[Environment] =
    IO {
      val properties = List(ToolkitProperty, "java.vendor", "java.vm.vendor", "java.runtime.name").flatMap(key =>
        Option(System.getProperty(key)).map(key -> _)
      )
      val osName                  = System.getProperty("os.name", "")
      val waylandToolkitAvailable = Option(ClassLoader.getSystemResource(WaylandToolkitClassFile)).nonEmpty
      val wantsWayland =
        sys.env.get("WAYLAND_DISPLAY").exists(_.trim.nonEmpty) ||
          sys.env.get(OverrideVariable).exists(_.trim.equalsIgnoreCase("wayland"))
      Environment(
        osName = osName,
        properties = properties.toMap,
        env = sys.env,
        waylandToolkitAvailable = waylandToolkitAvailable,
        waylandSocketProblem =
          if isLinux(osName) && waylandToolkitAvailable && wantsWayland then WaylandSocket.problem(sys.env) else None
      )
    }

  /** Must run before the first `java.awt` class is touched. */
  def install: IO[Decision] =
    currentEnvironment
      .map(choose)
      .flatTap(decision => IO(decision.choice.toolkitProperty.foreach(System.setProperty(ToolkitProperty, _))).void)
