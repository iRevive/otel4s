package org.typelevel.otel4s.sdk.metrics.internal.exemplar

import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData

import scala.concurrent.duration.FiniteDuration

private[internal] final case class Exemplar[A](
    filteredAttributes: Attributes,
    timestamp: FiniteDuration,
    traceContext: Option[ExemplarData.TraceContext],
    value: A
)
