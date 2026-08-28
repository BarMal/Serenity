package com.serenity.ui.renderer

import com.serenity.state.models.Damage

/** Stable identity for one independently-composited surface within a frame -- the base editor content, the chrome
  * (gutter/line numbers/spacers), the pinned-panel stack, the floating-overlay stack, or the modal layer. Distinct from
  * [[SurfaceContentIdentity]]/`Renderer`'s private `ScreenIdentity`, which identify pixel buffers and screens for
  * cross-frame damage bookkeeping; a `LayerId` identifies a *role* in a frame's paint order, stable across frames even
  * as the pixels behind it are redrawn.
  */
final case class LayerId(value: String)

/** Per-layer compositing parameters: how a layer's own rendered content is combined onto the frame it sits on. `alpha`
  * is the only knob today -- [[Renderer]] still paints every layer directly into the shared frame surface rather than
  * into a buffer of its own, so blur/tint/shadow have nowhere to attach yet. This is the seam #1086 (backdrop blur),
  * #1087 (per-surface glass) and #1091 (soft per-surface shadows) extend once each layer owns its own buffer.
  */
final case class LayerEffect(alpha: Float):
  require(alpha >= 0.0f && alpha <= 1.0f, s"LayerEffect.alpha must be in [0, 1], was $alpha")

object LayerEffect:

  /** No compositing adjustment: full opacity, nothing else applied. Every layer in today's frame uses this except the
    * modal backdrop's dimming.
    */
  val identity: LayerEffect = LayerEffect(1.0f)

/** One layer in a frame's composite stack: its position in paint order, the effect blended in as it is composited, and
  * the damage it has accumulated since it was last (re)rendered.
  *
  * A layer whose `damage` is [[Damage.Nothing]] does not need to be redrawn this frame -- once a layer owns a buffer
  * that persists across frames, the compositor can reuse whatever it drew last time instead of re-rendering. Nothing in
  * `Renderer` owns such a per-layer buffer yet (every layer still paints straight into the shared frame surface, so
  * skipping a layer would just leave a hole), which is why [[LayerCompositor.dirtyLayers]] is not yet called from the
  * render path -- it is exercised by its own tests, ready for the per-layer-buffer follow-up this issue's own
  * performance note asks for.
  */
final case class Layer(id: LayerId, zOrder: Int, effect: LayerEffect, damage: Damage)

/** The compositing seam #1100 introduces: today's single-pass paint sequence (spacer columns, line numbers, gutter,
  * editor panes, pinned panels, floating panels, modal) expressed as an explicit, ordered stack of named layers rather
  * than an implicit sequence of calls. `Renderer.renderFrame` builds that stack and paints each layer's already-written
  * function in the order [[orderedForComposite]] resolves, so the visual stacking order lives in one place as data
  * instead of being hard-coded by call sequence -- and the modal backdrop's alpha dimming, the one place today's
  * renderer already blends a layer, goes through [[withEffect]] rather than raw calls to `Effects.setAlpha`.
  */
object LayerCompositor:

  /** Layers in back-to-front paint order -- lower `zOrder` paints first, so later layers land on top. Stable for equal
    * `zOrder` values: layers that tie keep the order they were given in, so the stack's declared order controls
    * ambiguous cases rather than an unspecified sort.
    */
  def orderedForComposite(layers: List[Layer]): List[Layer] =
    layers.sortBy(_.zOrder)

  /** Layers that must be (re)rendered before this frame's composite -- every layer except one with empty damage, whose
    * last-rendered content is still correct and can simply be recomposited unchanged. See [[Layer]]'s doc comment for
    * why nothing calls this from `Renderer` yet.
    */
  def dirtyLayers(layers: List[Layer]): List[Layer] =
    layers.filterNot(layer => layer.damage == Damage.Nothing)

  /** Run `paint` with `effect`'s alpha applied to `surface`, restoring full opacity afterwards -- even if `paint`
    * throws, so one layer's effect never bleeds into the next layer painted on the same surface. A surface without an
    * [[Effects]] capability (e.g. a headless test double) simply runs `paint` at whatever alpha it already had, since
    * there is no alpha to set.
    */
  def withEffect(surface: RenderSurface)(effect: LayerEffect)(paint: => Unit): Unit =
    surface.effects match
      case None => paint
      case Some(fx) =>
        fx.setAlpha(effect.alpha)
        try paint
        finally fx.setAlpha(LayerEffect.identity.alpha)
