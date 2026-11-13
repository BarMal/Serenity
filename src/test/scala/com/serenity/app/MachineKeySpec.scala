package com.serenity.app

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MachineKeySpec extends AnyFlatSpec with Matchers:

  private val properties = Map("os.name" -> "Linux", "os.version" -> "6.18.44", "java.vm.version" -> "25.0.5+8-LTS")

  "MachineKey.of" should "name what can change which renderer is fastest, from what the process can read" in {
    val key = MachineKey.of(
      properties,
      Map("XDG_SESSION_TYPE" -> "x11"),
      display = Some("3840x2160@2.0"),
      skiko = Some("0.153.0"),
      onMains = Some(true)
    )
    key.os shouldBe "Linux 6.18.44"
    key.toolkit shouldBe "x11"
    key.display shouldBe "3840x2160@2.0"
    key.jvm shouldBe "25.0.5+8-LTS"
    key.skiko shouldBe "0.153.0"
    key.powerState shouldBe "ac"
  }

  it should "say battery when the machine is not on mains power, and unknown when it cannot tell" in {
    MachineKey.of(properties, Map.empty, None, None, onMains = Some(false)).powerState shouldBe "battery"
    MachineKey.of(properties, Map.empty, None, None, onMains = None).powerState shouldBe "unknown"
  }

  it should "say unknown for what it cannot read, rather than leave it out of the key" in {
    val key = MachineKey.of(Map.empty, Map.empty, None, None, None)
    List(key.os, key.toolkit, key.display, key.jvm, key.skiko, key.glRenderer) should contain only "unknown"
  }

  it should "not claim a GL renderer, which only a window can report" in {
    MachineKey.of(properties, Map.empty, None, None, None).glRenderer shouldBe "unknown"
  }

  it should "tell a Wayland session from an X11 one" in {
    MachineKey.of(properties, Map("XDG_SESSION_TYPE" -> "wayland"), None, None, None).toolkit shouldBe "wayland"
    MachineKey.of(properties, Map("WAYLAND_DISPLAY" -> "wayland-1"), None, None, None).toolkit shouldBe "wayland"
  }
