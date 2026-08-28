package com.serenity.ui.renderer

import com.serenity.state.models.{BufferId, Damage}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LayerCompositorSpec extends AnyFlatSpec with Matchers:

  private val someDamage = Damage.BufferRows(BufferId(1), Set(0))

  "LayerEffect" should "expose an identity effect at full opacity" in {
    LayerEffect.identity shouldBe LayerEffect(1.0f)
  }

  it should "reject an alpha outside [0, 1]" in {
    an[IllegalArgumentException] should be thrownBy LayerEffect(1.5f)
    an[IllegalArgumentException] should be thrownBy LayerEffect(-0.1f)
  }

  it should "accept the boundary values 0 and 1" in {
    noException should be thrownBy LayerEffect(0.0f)
    noException should be thrownBy LayerEffect(1.0f)
  }

  "LayerCompositor.orderedForComposite" should "sort layers back-to-front by zOrder" in {
    val back   = Layer(LayerId("back"), zOrder = 0, LayerEffect.identity, Damage.Nothing)
    val middle = Layer(LayerId("middle"), zOrder = 1, LayerEffect.identity, Damage.Nothing)
    val front  = Layer(LayerId("front"), zOrder = 2, LayerEffect.identity, Damage.Nothing)

    LayerCompositor.orderedForComposite(List(front, back, middle)) shouldBe List(back, middle, front)
  }

  it should "be stable for layers sharing the same zOrder" in {
    val first  = Layer(LayerId("first"), zOrder = 0, LayerEffect.identity, Damage.Nothing)
    val second = Layer(LayerId("second"), zOrder = 0, LayerEffect.identity, Damage.Nothing)

    LayerCompositor.orderedForComposite(List(first, second)) shouldBe List(first, second)
  }

  it should "return an empty list for an empty stack" in {
    LayerCompositor.orderedForComposite(Nil) shouldBe Nil
  }

  "LayerCompositor.dirtyLayers" should "keep a layer whose damage is non-empty" in {
    val layer = Layer(LayerId("editor"), zOrder = 0, LayerEffect.identity, someDamage)
    LayerCompositor.dirtyLayers(List(layer)) shouldBe List(layer)
  }

  it should "keep a layer whose damage is Everything" in {
    val layer = Layer(LayerId("editor"), zOrder = 0, LayerEffect.identity, Damage.Everything)
    LayerCompositor.dirtyLayers(List(layer)) shouldBe List(layer)
  }

  it should "drop a layer whose damage is Nothing, meaning its cached content is still correct" in {
    val layer = Layer(LayerId("pinned-panels"), zOrder = 1, LayerEffect.identity, Damage.Nothing)
    LayerCompositor.dirtyLayers(List(layer)) shouldBe Nil
  }

  it should "filter independently per layer, keeping order among the survivors" in {
    val dirty  = Layer(LayerId("editor"), zOrder = 0, LayerEffect.identity, someDamage)
    val clean  = Layer(LayerId("pinned-panels"), zOrder = 1, LayerEffect.identity, Damage.Nothing)
    val dirty2 = Layer(LayerId("modal"), zOrder = 2, LayerEffect.identity, Damage.Everything)

    LayerCompositor.dirtyLayers(List(dirty, clean, dirty2)) shouldBe List(dirty, dirty2)
  }

  "LayerCompositor.withEffect" should "run the paint block with the effect's alpha, then restore full opacity" in {
    val surface = new RecordingEffectsSurface()

    LayerCompositor.withEffect(surface)(LayerEffect(0.4f)) {
      surface.markPaintedAt(surface.alphaHistory.last)
    }

    surface.alphaHistory shouldBe List(0.4f, 1.0f)
    surface.paintedAtAlpha shouldBe Some(0.4f)
  }

  it should "still restore full opacity even when the paint block throws" in {
    val surface = new RecordingEffectsSurface()

    a[RuntimeException] should be thrownBy
      LayerCompositor.withEffect(surface)(LayerEffect(0.5f)) {
        throw new RuntimeException("boom")
      }

    surface.alphaHistory shouldBe List(0.5f, 1.0f)
  }

  it should "skip alpha calls entirely when the surface has no Effects capability" in {
    val painted = new java.util.concurrent.atomic.AtomicBoolean(false)
    val surface = new NoEffectsSurface()

    LayerCompositor.withEffect(surface)(LayerEffect(0.4f)) {
      painted.set(true)
    }

    painted.get() shouldBe true
  }

  /** A minimal test double exposing only what [[LayerCompositor.withEffect]] needs: an `Effects` capability that
    * records every `setAlpha` call, and a `RenderSurface` wrapping it.
    */
  private class RecordingEffectsSurface extends RenderSurface with Effects:
    private val alphaCalls                = new java.util.concurrent.atomic.AtomicReference[List[Float]](Nil)
    private val painted                   = new java.util.concurrent.atomic.AtomicReference[Option[Float]](None)
    def alphaHistory: List[Float]         = alphaCalls.get()
    def paintedAtAlpha: Option[Float]     = painted.get()
    def markPaintedAt(alpha: Float): Unit = painted.set(Some(alpha))

    override def effects: Option[Effects]                                        = Some(this)
    def setAlpha(alpha: Float): Unit                                             = alphaCalls.updateAndGet(_ :+ alpha)
    def blurRegion(x: Int, y: Int, width: Int, height: Int, radius: Float): Unit = ()

    def applyPostProcessing(
      effect: com.serenity.config.PostProcessingEffect,
      animationPhase: Long = 0L
    ): Unit = ()

    def setForegroundColor(color: java.awt.Color): Unit                     = ()
    def setBackgroundColor(color: java.awt.Color): Unit                     = ()
    def getBackgroundColor: java.awt.Color                                  = java.awt.Color.BLACK
    def putString(x: Int, y: Int, s: String): Unit                          = ()
    def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit = ()
    def enableStyle(style: com.serenity.ui.theme.TextStyle): Unit           = ()
    def disableStyle(style: com.serenity.ui.theme.TextStyle): Unit          = ()
    def text: TextDrawing    = throw new UnsupportedOperationException("not needed for this test")
    def pixels: PixelDrawing = throw new UnsupportedOperationException("not needed for this test")
    def hideCursor(): Unit   = ()
    def viewportWidth: Int   = 0
    def viewportHeight: Int  = 0
    def flush(): Unit        = ()

  private class NoEffectsSurface extends RenderSurface:
    def setForegroundColor(color: java.awt.Color): Unit                     = ()
    def setBackgroundColor(color: java.awt.Color): Unit                     = ()
    def getBackgroundColor: java.awt.Color                                  = java.awt.Color.BLACK
    def putString(x: Int, y: Int, s: String): Unit                          = ()
    def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit = ()
    def enableStyle(style: com.serenity.ui.theme.TextStyle): Unit           = ()
    def disableStyle(style: com.serenity.ui.theme.TextStyle): Unit          = ()
    def text: TextDrawing    = throw new UnsupportedOperationException("not needed for this test")
    def pixels: PixelDrawing = throw new UnsupportedOperationException("not needed for this test")
    def hideCursor(): Unit   = ()
    def viewportWidth: Int   = 0
    def viewportHeight: Int  = 0
    def flush(): Unit        = ()
