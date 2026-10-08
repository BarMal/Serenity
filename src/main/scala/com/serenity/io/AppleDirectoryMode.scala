package com.serenity.io

/** macOS AWT `FileDialog` picks directories only while `apple.awt.fileDialogForDirectories` is set, and that is one
  * JVM-wide property read each time a dialog opens. Every macOS dialog therefore sets it explicitly for its own
  * duration and puts back what it found. Dialogs are shown on the event dispatch thread, where a second one can only
  * start inside the first one's modal loop, so the saves and restores nest and cannot interleave.
  */
private[io] object AppleDirectoryMode:

  val Key = "apple.awt.fileDialogForDirectories"

  final case class Properties(read: String => Option[String], write: (String, Option[String]) => Unit)

  object Properties:

    val jvm: Properties = Properties(
      read = name => Option(System.getProperty(name)),
      write = (name, value) =>
        value match
          case Some(text) => val _ = System.setProperty(name, text)
          case None       => val _ = System.clearProperty(name)
    )

  def around[A](forDirectories: Boolean, properties: Properties = Properties.jvm)(body: => A): A =
    val previous = properties.read(Key)
    properties.write(Key, Some(forDirectories.toString))
    try body
    finally properties.write(Key, previous)

  def appliesTo(osName: String): Boolean =
    osName.toLowerCase(java.util.Locale.ROOT).contains("mac")
