package com.serenity.io

/** The few Objective-C runtime operations the open panel needs, so what is sent can be checked against a recording
  * double. Every object, selector, BOOL and NSInteger travels as a `Long`: on the 64-bit macOS ABIs (arm64 and x86-64)
  * those are all passed and returned as one integer register, which is what lets `objc_msgSend` be called with one
  * signature. `0` is `nil`.
  */
private[io] trait ObjectiveC:
  def classNamed(name: String): Long

  def send(receiver: Long, selector: String, arguments: Long*): Long

  /** `text` as a NUL-terminated UTF-8 C string that is valid only while `use` runs. */
  def withUtf8[A](text: String)(use: Long => A): A

  def readUtf8(cString: Long): Option[String]
