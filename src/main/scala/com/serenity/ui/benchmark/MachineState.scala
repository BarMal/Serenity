package com.serenity.ui.benchmark

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.sys.process.Process
import scala.util.Try

/** One entry of `/sys/class/power_supply`: its `type` and, for a supply that reports it, whether it is `online`. */
final case class PowerSupply(kind: String, online: Option[Boolean])

/** What a run's figures depend on besides the code: the CPU, the frequency governor and whether the machine was on
  * mains. A field is "unknown" where the platform does not say.
  */
final case class MachineState(cpuModel: String, cpuGovernor: String, power: String)

object MachineState:

  private val unknown = "unknown"

  val Unknown: MachineState = MachineState(unknown, unknown, unknown)

  def cpuModelFromCpuinfo(cpuinfo: String): Option[String] =
    cpuinfo.linesIterator
      .map(_.split(":", 2))
      .collectFirst { case Array(key, value) if key.trim == "model name" && value.trim.nonEmpty => value.trim }

  def powerFrom(supplies: List[PowerSupply]): String =
    if supplies.exists(s => s.kind == "Mains" && s.online.contains(true)) then "AC"
    else if supplies.exists(_.kind == "Battery") then "battery"
    else unknown

  def current: MachineState =
    val os = System.getProperty("os.name", "").toLowerCase
    if os.contains("linux") then linux
    else if os.contains("windows") then
      Unknown.copy(cpuModel = Option(System.getenv("PROCESSOR_IDENTIFIER")).filter(_.nonEmpty).getOrElse(unknown))
    else if os.contains("mac") then
      Unknown.copy(cpuModel = command(Seq("sysctl", "-n", "machdep.cpu.brand_string")).getOrElse(unknown))
    else Unknown

  private def linux: MachineState =
    MachineState(
      cpuModel = read(Paths.get("/proc/cpuinfo")).flatMap(cpuModelFromCpuinfo).getOrElse(unknown),
      cpuGovernor = read(Paths.get("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor")).getOrElse(unknown),
      power = powerFrom(linuxSupplies)
    )

  private def linuxSupplies: List[PowerSupply] =
    val root = Paths.get("/sys/class/power_supply")
    Try(Files.list(root).iterator.asScala.toList).getOrElse(Nil).flatMap { dir =>
      read(dir.resolve("type")).map(kind => PowerSupply(kind, read(dir.resolve("online")).map(_ == "1")))
    }

  private def read(path: Path): Option[String] =
    Try(Files.readString(path).trim).toOption.filter(_.nonEmpty)

  private def command(args: Seq[String]): Option[String] =
    Try(Process(args).!!.trim).toOption.filter(_.nonEmpty)
