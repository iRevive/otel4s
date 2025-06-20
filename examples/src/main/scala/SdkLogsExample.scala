/*
 * Copyright 2022 Typelevel
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

import cats.Monad
import cats.effect.{IO, IOApp}
import cats.mtl.Local
import cats.syntax.all._
import org.typelevel.otel4s.logs.{LoggerProvider, Severity}
import org.typelevel.otel4s.sdk.OpenTelemetrySdk
import org.typelevel.otel4s.sdk.context.{Context, LocalContext}
import org.typelevel.otel4s.sdk.exporter.otlp.autoconfigure.OtlpExportersAutoConfigure
import org.typelevel.otel4s.semconv.attributes.CodeAttributes
import org.typelevel.otel4s.{AnyValue, Attribute, Attributes}

import scala.concurrent.duration._
import scala.util.chaining._

object SdkLogsExample extends IOApp.Simple {

  def run: IO[Unit] =
    OpenTelemetrySdk
      .autoConfigured[IO](_.addExportersConfigurer(OtlpExportersAutoConfigure[IO]))
      .use { otel4s =>
        implicit val ctx: LocalContext[IO] = otel4s.sdk.localContext
        val logger = new ScriberLoggerSupport[IO](otel4s.sdk.loggerProvider)

        otel4s.sdk.tracerProvider.get("tracer").flatMap { tracer =>
          tracer.spanBuilder("test-span").build.surround {
            logger.info("my first log message from scribe")
          }
        }
      }

  import scribe._

  private class ScriberLoggerSupport[F[_]: Monad: LocalContext](tp: LoggerProvider[F, Context])
      extends LoggerSupport[F[Unit]] {
    def log(record: => LogRecord): F[Unit] = {
      for {
        logger <- tp.get(record.className)
        ctx <- Local[F, Context].ask
        r = record
        _ <- logger.logRecordBuilder
          // severity
          .pipe { l =>
            toSeverity(r.level).fold(l)(l.withSeverity)
          }
          .withSeverityText(r.level.name)
          // timestmap
          .withTimestamp(r.timeStamp.millis)
          // log message
          .withBody(AnyValue.string(r.logOutput.plainText))
          // tracing ctx
          .withContext(ctx)
          // thread info
          .pipe { l =>
            l.addAttributes(
              if (r.thread.getId != -1) {
                Attributes(
                  Attribute("thread.id", r.thread.getId),
                  Attribute("thread.name", r.thread.getName),
                )
              } else {
                Attributes(
                  Attribute("thread.name", r.thread.getName),
                )
              }
            )
          }
          // code path info
          .pipe { l =>
            l.addAttributes(
              Attributes(
                Attribute("code.namespace", r.className),
                CodeAttributes.CodeFilePath(r.fileName)
              ) ++
                r.line.map(line => CodeAttributes.CodeLineNumber(line.toLong)).to(Attributes) ++
                r.column.map(col => CodeAttributes.CodeColumnNumber(col.toLong)).to(Attributes) ++
                r.methodName.map(name => CodeAttributes.CodeFunctionName(name)).to(Attributes)
            )
          }
          // todo: MDC context
          /*.pipe { l =>
            r.data

            r.fileName

          }*/
          .emit
      } yield ()
    }

    private def toSeverity(level: Level): Option[Severity] =
      level match {
        case Level("TRACE", _) => Some(Severity.Trace.trace1)
        case Level("DEBUG", _) => Some(Severity.Debug.debug1)
        case Level("INFO", _)  => Some(Severity.Info.info1)
        case Level("WARN", _)  => Some(Severity.Warn.warn1)
        case Level("ERROR", _) => Some(Severity.Error.error1)
        case Level("FATAL", _) => Some(Severity.Fatal.fatal1)
        case _                 => None
      }
  }

}
