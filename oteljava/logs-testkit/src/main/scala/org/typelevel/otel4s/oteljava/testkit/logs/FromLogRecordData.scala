/*
 * Copyright 2025 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.otel4s.oteljava.testkit.logs

import io.opentelemetry.sdk.logs.data.LogRecordData
import org.typelevel.otel4s.oteljava.testkit.logs.data.LogRecord

/** Transforms OpenTelemetry's LogRecordData into arbitrary type `A`.
  */
sealed trait FromLogRecordData[A] {
  def from(logRecordData: LogRecordData): A
}

object FromLogRecordData {

  def apply[A](implicit ev: FromLogRecordData[A]): FromLogRecordData[A] = ev

  implicit val toOtelJavaLogRecordData: FromLogRecordData[LogRecordData] =
    new FromLogRecordData[LogRecordData] {
      def from(logRecordData: LogRecordData): LogRecordData = logRecordData
    }

  implicit val toOtel4sLogRecord: FromLogRecordData[LogRecord] =
    new FromLogRecordData[LogRecord] {
      def from(logRecordData: LogRecordData): LogRecord = LogRecord(logRecordData)
    }

}
