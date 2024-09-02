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

package org.typelevel.otel4s.semconv.experimental.attributes

import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.AttributeKey._

// DO NOT EDIT, this is an Auto-generated file from buildscripts/semantic-convention/templates/SemanticAttributes.scala.j2
object UserExperimentalAttributes {

  /** User email address.
    */
  val UserEmail: AttributeKey[String] = string("user.email")

  /** User's full name
    */
  val UserFullName: AttributeKey[String] = string("user.full_name")

  /** Unique user hash to correlate information for a user in anonymized form.
    *
    * @note
    *   - Useful if `user.id` or `user.name` contain confidential information
    *     and cannot be used.
    */
  val UserHash: AttributeKey[String] = string("user.hash")

  /** Unique identifier of the user.
    */
  val UserId: AttributeKey[String] = string("user.id")

  /** Short name or login/username of the user.
    */
  val UserName: AttributeKey[String] = string("user.name")

  /** Array of user roles at the time of the event.
    */
  val UserRoles: AttributeKey[Seq[String]] = stringSeq("user.roles")

}
