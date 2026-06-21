package com.serenity.ui.display

import java.awt.{Component, GraphicsEnvironment}

object DisplayScale:

  case class DeviceScale(x: Double, y: Double):
    def textScale: Double =
      x.max(y).max(1.0)

  val One: DeviceScale = DeviceScale(1.0, 1.0)

  def forComponent(component: Component): DeviceScale =
    Option(component.getGraphicsConfiguration)
      .map(_.getDefaultTransform)
      .map(transform => DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0)))
      .getOrElse(One)

  def defaultDeviceScale: DeviceScale =
    try
      val config    = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
      val transform = config.getDefaultTransform
      DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0))
    catch case _: Exception => One
