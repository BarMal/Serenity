package com.serenity.input

import java.awt.datatransfer.{DataFlavor, Transferable, UnsupportedFlavorException}

/** A drag's payload without AWT's drag machinery: each offered flavor maps to what asking for it yields. */
final class FakeTransferable(offered: List[(DataFlavor, () => AnyRef)]) extends Transferable:

  def getTransferDataFlavors: Array[DataFlavor] = offered.map(_._1).toArray

  def isDataFlavorSupported(flavor: DataFlavor): Boolean = offered.exists(_._1.equals(flavor))

  def getTransferData(flavor: DataFlavor): AnyRef =
    offered.collectFirst { case (offeredFlavor, data) if offeredFlavor.equals(flavor) => data() } match
      case Some(data) => data
      case None       => throw new UnsupportedFlavorException(flavor)
