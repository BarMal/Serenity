package com.serenity.input

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.lang.ref.WeakReference
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

/** The decisions behind the window's drop target, kept apart from AWT's drag events so they can be tested without a
  * display. `onOffered` is told what each drag offers when it enters, which is what shows why a drop did nothing on a
  * toolkit that offers a flavor nobody expected.
  */
final case class FileDropHandler(onDropped: List[Path] => Unit, onOffered: String => Unit)(
    private val lastTaken: AtomicReference[WeakReference[Transferable]] = new AtomicReference(new WeakReference(null))
):

  def accepts(flavors: Array[DataFlavor]): Boolean = DroppedPaths.offersPaths(flavors)

  def entered(flavors: Array[DataFlavor]): Boolean =
    onOffered(FileDropHandler.describe(flavors))
    accepts(flavors)

  /** Whether the drop gave the editor anything to open. The same drop reaching the handler again, as it can through
    * nested drop targets, is reported as taken but not opened a second time; a new drag is a new [[Transferable]].
    */
  def handle(transferable: Transferable): Boolean =
    DroppedPaths.from(transferable) match
      case Nil => false
      case paths =>
        val previous = lastTaken.getAndSet(new WeakReference(transferable))
        if previous.get ne transferable then onDropped(paths)
        true

object FileDropHandler:

  def describe(flavors: Array[DataFlavor]): String =
    if flavors.isEmpty then "Drag offers no flavors"
    else s"Drag offers: ${flavors.map(_.getMimeType).mkString(", ")}"
