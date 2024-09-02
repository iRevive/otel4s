/*
 * Copyright 2023 Typelevel
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

package org.typelevel.otel4s.sdk.resource

import cats.effect.Sync
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.semconv.SchemaUrls

import scala.scalanative.posix.unistd._
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

private[resource] trait HostDetectorPlatform { self: HostDetector.type =>

  def apply[F[_]: Sync]: TelemetryResourceDetector[F] =
    new Detector[F]

  private class Detector[F[_]: Sync] extends TelemetryResourceDetector[F] {
    def name: String = Const.Name

    def detect: F[Option[TelemetryResource]] =
      for {
        hostOpt <- Sync[F].delay(detectHost).handleError(_ => None)
        archOpt <- Sync[F].delay(sys.props.get("os.arch"))
      } yield {
        val host = hostOpt.map(Keys.Host(_))
        val arch = archOpt.map(Keys.Arch(_))
        val attributes = host.to(Attributes) ++ arch.to(Attributes)
        Option.when(attributes.nonEmpty)(
          TelemetryResource(attributes, Some(SchemaUrls.Current))
        )
      }

    private def detectHost: Option[String] =
      Zone { implicit z =>
        val size = 256.toUInt
        val hostnameBuffer = alloc[CChar](size)
        if (gethostname(hostnameBuffer, size) == 0) {
          val hostname = fromCString(hostnameBuffer)
          Some(hostname)
        } else {
          None
        }
      }
  }

}
