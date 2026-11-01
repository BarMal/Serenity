package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MachineStateSpec extends AnyFlatSpec with Matchers:

  "MachineState.cpuModelFromCpuinfo" should "take the first model name" in {
    val cpuinfo =
      "processor\t: 0\nmodel name\t: AMD Ryzen 7 7840U\ncpu MHz\t: 400\nprocessor\t: 1\nmodel name\t: other\n"
    MachineState.cpuModelFromCpuinfo(cpuinfo) shouldBe Some("AMD Ryzen 7 7840U")
  }

  it should "give nothing for text with no model name" in {
    MachineState.cpuModelFromCpuinfo("processor\t: 0\n") shouldBe None
  }

  "MachineState.powerFrom" should "say AC when a mains supply is online" in {
    MachineState.powerFrom(List(PowerSupply("Battery", None), PowerSupply("Mains", Some(true)))) shouldBe "AC"
  }

  it should "say battery when a battery exists and no mains supply is online" in {
    MachineState.powerFrom(List(PowerSupply("Battery", None), PowerSupply("Mains", Some(false)))) shouldBe "battery"
  }

  it should "say AC for a machine with a mains supply and no battery" in {
    MachineState.powerFrom(List(PowerSupply("Mains", Some(true)))) shouldBe "AC"
  }

  it should "say unknown when there are no supplies to read" in {
    MachineState.powerFrom(Nil) shouldBe "unknown"
  }

  "MachineState.Unknown" should "name every field unknown" in {
    MachineState.Unknown shouldBe MachineState("unknown", "unknown", "unknown")
  }

  "MachineState.current" should "always produce a non-empty value for each field" in {
    val state = MachineState.current
    List(state.cpuModel, state.cpuGovernor, state.power).foreach(_.trim should not be empty)
  }
