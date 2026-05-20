package com.serenity.state.models

import java.nio.file.Path

import com.serenity.rope.Rope

case class BufferId(value: Int)

case class Buffer(
    id: BufferId,
    content: Rope,
    filePath: Option[Path] = None,
    isDirty: Boolean = false,
    language: Option[String] = None
)

object Buffer:
  def empty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope.empty)

  def fromString(id: BufferId, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope(content))

  def fromFile(id: BufferId, path: Path, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope(content), Some(path))
