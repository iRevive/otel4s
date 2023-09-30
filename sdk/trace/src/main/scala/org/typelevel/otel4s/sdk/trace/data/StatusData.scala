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

package org.typelevel.otel4s.sdk.trace.data

import org.typelevel.otel4s.trace.Status

final case class StatusData(status: Status, description: String)

object StatusData {

  val Ok = StatusData(Status.Ok, "")
  val Unset = StatusData(Status.Unset, "")
  val Error = StatusData(Status.Error, "")

  def create(status: Status): StatusData =
    status match {
      case Status.Ok    => Ok
      case Status.Unset => Unset
      case Status.Error => Error
    }

  def create(status: Status, description: String): StatusData =
    if (description.isEmpty) create(status)
    else StatusData(status, description)

}
