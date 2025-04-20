package org.typelevel.otel4s.sdk.logs.exporter

import cats.Foldable
import org.typelevel.otel4s.sdk.logs.data.LogRecordData

/** An interface for exporting `LogRecordData`.
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/sdk/#logrecordexporter]]
  *
  * @tparam F
  *   the higher-kinded type of polymorphic effect
  */
trait LogRecordExporter[F[_]] {

  /** The name of the exporter.
    */
  def name: String

  /** Exports the collection of `LogRecordData`.
    *
    * @param logs
    *   the sampled metrics to export
    */
  def exportLogs[G[_]: Foldable](logs: G[LogRecordData]): F[Unit]

  /** Exports the collection of sampled `MetricData` that have not yet been exported.
    */
  def flush: F[Unit]

  override def toString: String =
    name

}
