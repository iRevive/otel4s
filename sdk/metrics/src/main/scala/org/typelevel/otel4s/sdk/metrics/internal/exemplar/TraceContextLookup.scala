/*
 * Copyright 2024 Typelevel
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

package org.typelevel.otel4s.sdk.metrics.internal.exemplar

import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData.TraceContext

// todo: package may be different
trait TraceContextLookup {
  def get(context: Context): Option[TraceContext]
  def isSampled(context: Context): Boolean
}

object TraceContextLookup {
  def noop[F[_]]: TraceContextLookup =
    new TraceContextLookup {
      def get(context: Context): Option[TraceContext] = None
      def isSampled(context: Context): Boolean = false
    }
}
