package com.serenity.markdown

import java.awt.image.BufferedImage
import java.awt.{Font, RenderingHints}
import java.io.{ByteArrayInputStream, StringReader}
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import javax.imageio.ImageIO
import javax.xml.parsers.DocumentBuilderFactory

import scala.util.Try

import com.serenity.ui.theme.Theme
import org.w3c.dom.Document
import org.xhtmlrenderer.resource.ImageResource
import org.xhtmlrenderer.swing.{AWTFSImageFactory, ImageResourceLoader, SwingReplacedElementFactory}
import org.xml.sax.InputSource

/** Turns the XHTML fragment produced for a preview into a laid-out image, and guards every image reference the fragment
  * can contain along the way: local files are confined to the preview's resource root (defeating symlink and `..`
  * traversal), remote and data-URI images never reach flying-saucer's decoder, and oversized payloads are rejected
  * before decoding.
  */
private[markdown] object MarkdownPreviewImageResources:

  private val MaxImageBytes     = 2 * 1024 * 1024
  private val MaxImageDimension = 4096
  private val MaxImagePixels    = MaxImageDimension.toLong * MaxImageDimension.toLong

  def parseXhtml(xhtml: String): Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.newDocumentBuilder().parse(InputSource(StringReader(xhtml)))

  final class PreviewResourcePolicy(baseUri: Option[URI]):

    private val resourceRoot = baseUri
      .filter(uri => uri.getScheme == "file" && uri.getHost == null)
      .flatMap(uri => Try(Paths.get(uri).toAbsolutePath.normalize()).toOption)

    def isDataUri(uri: String): Boolean =
      Option(uri).exists(_.trim.toLowerCase(Locale.ROOT).startsWith("data:"))

    def placeholderImage: BufferedImage =
      new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)

    def imageFor(uri: String): BufferedImage =
      loadImage(uri).getOrElse(placeholderImage)

    private def loadImage(uri: String): Option[BufferedImage] =
      for
        parsed <- Try(URI.create(uri)).toOption
        path   <- permittedFile(parsed)
        bytes  <- readBounded(path)
        image  <- decodeImage(bytes)
      yield image

    private def permittedFile(uri: URI): Option[Path] =
      Option
        .when(uri.getScheme == "file" && uri.getHost == null && resourceRoot.nonEmpty) {
          Try(Paths.get(uri).toAbsolutePath.normalize()).toOption
        }
        .flatten
        .filter { path =>
          resourceRoot.exists { root =>
            path.startsWith(root) &&
            Try(path.toRealPath().startsWith(root.toRealPath())).getOrElse(false) &&
            Files.isRegularFile(path)
          }
        }

    private def readBounded(path: Path): Option[Array[Byte]] =
      Try {
        val input = Files.newInputStream(path)
        try
          val bytes = input.readNBytes(MaxImageBytes + 1)
          if bytes.length <= MaxImageBytes then Some(bytes) else None
        finally input.close()
      }.toOption.flatten

    private def decodeImage(bytes: Array[Byte]): Option[BufferedImage] =
      Try {
        Option(ImageIO.createImageInputStream(new ByteArrayInputStream(bytes))).flatMap { input =>
          val readers = ImageIO.getImageReaders(input)
          if !readers.hasNext then
            input.close()
            None
          else
            val reader = readers.next()
            try
              reader.setInput(input, true, true)
              val width  = reader.getWidth(0)
              val height = reader.getHeight(0)
              Option
                .when(
                  width > 0 &&
                    height > 0 &&
                    width <= MaxImageDimension &&
                    height <= MaxImageDimension &&
                    width.toLong * height.toLong <= MaxImagePixels
                )(Option(ImageIO.read(new ByteArrayInputStream(bytes))))
                .flatten
            finally
              reader.dispose()
              input.close()
        }
      }.toOption.flatten

  def previewReplacedElementFactory(resourcePolicy: PreviewResourcePolicy): SwingReplacedElementFactory =
    new PreviewReplacedElementFactory(resourcePolicy)

  private class PreviewReplacedElementFactory(resourcePolicy: PreviewResourcePolicy)
      extends SwingReplacedElementFactory(
        ImageResourceLoader.NO_OP_REPAINT_LISTENER,
        new ImageResourceLoader:
          override def get(uri: String, width: Int, height: Int): ImageResource =
            ImageResource(uri, AWTFSImageFactory.createImage(resourcePolicy.imageFor(uri)))
      ):

    override def createReplacedElement(
      context: org.xhtmlrenderer.layout.LayoutContext,
      box: org.xhtmlrenderer.render.BlockBox,
      userAgent: org.xhtmlrenderer.extend.UserAgentCallback,
      cssWidth: Int,
      cssHeight: Int
    ): org.xhtmlrenderer.extend.ReplacedElement =
      val element = Option(box.getElement)
      val dataImage = element
        .filter(_.getNodeName.equalsIgnoreCase("img"))
        .map(_.getAttribute("src"))
        .exists(resourcePolicy.isDataUri)
      if dataImage then
        new org.xhtmlrenderer.swing.InstantImageReplacedElement(
          resourcePolicy.placeholderImage,
          cssWidth,
          cssHeight
        )
      else super.createReplacedElement(context, box, userAgent, cssWidth, cssHeight)

  def fallbackImage(width: Int, height: Int, theme: Theme, font: Font, message: String): BufferedImage =
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setColor(theme.panel.background)
      g.fillRect(0, 0, width, height)
      g.setColor(theme.error.foreground)
      g.setFont(font)
      g.drawString("Markdown preview failed", 16, 28)
      g.setColor(theme.panel.foreground)
      Option(message).foreach(text => g.drawString(text.take(120), 16, 50))
    finally g.dispose()
    image
