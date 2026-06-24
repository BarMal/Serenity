package com.serenity.ui.display

import java.awt.geom.AffineTransform
import java.awt.{Component, GraphicsConfiguration, GraphicsEnvironment}

object DisplayScale:

  case class DeviceScale(x: Double, y: Double):
    def textScale: Double =
      x.max(y).max(1.0)

  val One: DeviceScale = DeviceScale(1.0, 1.0)

  def fromTransform(transform: AffineTransform): DeviceScale =
    DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0))

  def fromGraphicsConfiguration(configuration: GraphicsConfiguration): DeviceScale =
    fromTransform(configuration.getDefaultTransform)

  def forComponent(component: Component): DeviceScale =
    Option(component.getGraphicsConfiguration)
      .map(fromGraphicsConfiguration)
      .getOrElse(One)

  def defaultDeviceScale: DeviceScale =
    try
      val config = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
      fromGraphicsConfiguration(config)
    catch case _: Exception => One
