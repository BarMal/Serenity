package com.serenity.ui.skia

import com.serenity.ui.renderer.DeviceArea

/** Sets of device pixels as lists of areas: what the copy-out plan reasons in. */
private[skia] object DeviceAreas:

  def intersect(a: DeviceArea, b: DeviceArea): DeviceArea =
    val left   = a.left.max(b.left)
    val top    = a.top.max(b.top)
    val right  = (a.left + a.width).min(b.left + b.width)
    val bottom = (a.top + a.height).min(b.top + b.height)
    DeviceArea(left, top, (right - left).max(0), (bottom - top).max(0))

  def intersect(a: List[DeviceArea], b: List[DeviceArea]): List[DeviceArea] =
    for
      first  <- a
      second <- b
      common = intersect(first, second)
      if !common.isEmpty
    yield common

  /** The pixels of `from` that none of `removed` holds. */
  def subtract(from: List[DeviceArea], removed: List[DeviceArea]): List[DeviceArea] =
    removed.foldLeft(from.filterNot(_.isEmpty))((remaining, gone) => remaining.flatMap(minus(_, gone)))

  private def minus(area: DeviceArea, gone: DeviceArea): List[DeviceArea] =
    val common = intersect(area, gone)
    if common.isEmpty then List(area)
    else
      val right  = area.left + area.width
      val bottom = area.top + area.height
      List(
        DeviceArea(area.left, area.top, area.width, common.top - area.top),
        DeviceArea(area.left, common.top + common.height, area.width, bottom - common.top - common.height),
        DeviceArea(area.left, common.top, common.left - area.left, common.height),
        DeviceArea(common.left + common.width, common.top, right - common.left - common.width, common.height)
      ).filterNot(_.isEmpty)

  def shifted(area: DeviceArea, dyPx: Int): DeviceArea = area.copy(top = area.top + dyPx)

  def within(areas: List[DeviceArea], whole: DeviceArea): List[DeviceArea] = intersect(areas, List(whole))
