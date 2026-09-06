package com.serenity.animation.sprite

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/** Loads a character's sprite sheet from the classpath and slices its declared clips into per-frame images.
  *
  * A generated placeholder ships for [[CompanionCharacter.PixelWizard]] (`/sprites/pixel-wizard-idle.png`, a 4-frame
  * 16x16 idle bob-cycle) -- a hand-drawn or licensed replacement can drop in under the same resource path and layout
  * without any code change here.
  */
object CompanionSpriteAssets:

  val IdleClipName: String = "idle"
  val IdleFrameSize: Int   = 16
  val IdleFrameCount: Int  = 4
  val IdleLayout: SpriteSheetLayout =
    SpriteSheetLayout.horizontalStrip(IdleClipName, IdleFrameSize, IdleFrameSize, IdleFrameCount)

  /** `None` when the classpath resource is missing or fails to decode -- mirrors `SwingWindow.applicationIconImages`'s
    * own `Option(...).flatMap(...)` shape for loading a bundled image, so a companion pane with no loadable sheet
    * degrades to simply not painting rather than crashing render.
    */
  def loadSheet(character: CompanionCharacter): Option[BufferedImage] =
    Option(getClass.getResource(character.sheetResourcePath)).flatMap(url => Option(ImageIO.read(url)))

  /** Every clip the sheet actually has frames for, sliced and ready to paint. Only [[IdleLayout]] is declared today, so
    * this is always either empty (sheet failed to load) or `Map("idle" -> <4 frames>)`; painting a non-idle
    * [[CompanionSpriteAction]] before a matching clip exists falls back to these idle frames (see
    * [[CompanionSpriteFrames.framesFor]]).
    *
    * Backed by [[framesByCharacter]], computed once per character rather than re-decoding the sheet on every render
    * call.
    */
  def loadFrames(character: CompanionCharacter): Map[String, Vector[BufferedImage]] =
    framesByCharacter.getOrElse(character, Map.empty)

  private def loadFramesFromDisk(character: CompanionCharacter): Map[String, Vector[BufferedImage]] =
    loadSheet(character).fold(Map.empty[String, Vector[BufferedImage]])(sheet =>
      SpriteSheetSlicer.slice(sheet, IdleLayout)
    )

  /** Every bundled character's sheet, decoded once at class-init time -- `CompanionCharacter` is a small closed enum,
    * so eagerly loading all of them costs nothing render loops would notice, and avoids `ImageIO.read`ing the same
    * classpath resource on every single frame paint (mirrors `SwingWindow.applicationIconImages`'s own eager,
    * decoded-once `lazy val`).
    */
  private lazy val framesByCharacter: Map[CompanionCharacter, Map[String, Vector[BufferedImage]]] =
    CompanionCharacter.values.map(character => character -> loadFramesFromDisk(character)).toMap

/** Resolves a [[CompanionSpriteState]] to the frame it should currently paint, given a sheet's actually-loaded clips.
  */
object CompanionSpriteFrames:

  def framesFor(
    clipsByName: Map[String, Vector[BufferedImage]],
    action: CompanionSpriteAction
  ): Vector[BufferedImage] =
    clipsByName
      .get(action.toString.toLowerCase)
      .orElse(clipsByName.get(CompanionSpriteAssets.IdleClipName))
      .getOrElse(Vector.empty)

  def currentFrame(
    clipsByName: Map[String, Vector[BufferedImage]],
    state: CompanionSpriteState
  ): Option[BufferedImage] =
    val frames = framesFor(clipsByName, state.action)
    if frames.isEmpty then None else Some(frames(state.frameIndex % frames.length))
