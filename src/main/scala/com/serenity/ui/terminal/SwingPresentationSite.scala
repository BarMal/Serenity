package com.serenity.ui.terminal

import javax.swing.JPanel

import com.serenity.ui.renderer.PresentationSite

/** A Swing window's canvas, the one place a Skia layer can be hosted today. */
final case class SwingPresentationSite(panel: JPanel) extends PresentationSite
