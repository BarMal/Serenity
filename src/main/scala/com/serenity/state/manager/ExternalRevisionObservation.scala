package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.state.models.BufferId

/** A buffer's file seen on disk at a revision other than the one the buffer held when it was read (#1623). */
final private[manager] case class ExternalRevisionObservation(
    bufferId: BufferId,
    path: Path,
    bufferRevision: Option[com.serenity.io.DocumentRevision],
    onDisk: com.serenity.io.DocumentRevision
)
