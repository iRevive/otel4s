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

package org.typelevel.otel4s.sdk.testkit.trace.instances

import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.testkit.trace.SpanTree

trait SpanLikeInstances {

  implicit val spanDataSpanLike: SpanTree.SpanLike[SpanData] =
    SpanTree.SpanLike.make(
      _.spanContext.spanIdHex,
      _.parentSpanContext.filter(_.isValid).map(_.spanIdHex)
    )

}
