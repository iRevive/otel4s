package org.typelevel.otel4s

import scodec.bits.ByteVector

trait TraceContext {
  def traceId: ByteVector
  def spanId: ByteVector
  def isSampled: Boolean
}
