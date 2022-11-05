/*
 * Copyright (c) 2022, Nigel Eke
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package life

/** Bounds for a board or viewport.
  * @param rows
  *   Range for the row boundary.
  * @param columns
  *   Range for the column boundary.
  */
final case class Bounds(rows: Range, columns: Range)

object Bounds:
  import com.typesafe.config.*

  /** Extract [Bounds] from a [Config]. The [Config] must be the root for the [Bounds] definition. The definition requires each of
    * `minRow`, `maxRow`, `minColumn` and `maxColumn` to be defined, e.g.,
    *
    * ```
    * mybounds {
    *   minRow: 0
    *   maxRow: 40
    *   minColumn: 0
    *   maxColumn: 80
    * }
    * ```
    *
    * @param config
    *   The root [Config] object
    * @return
    *   The [Bounds] object derived from the [Config] settings.
    */
  def from(config: Config) =
    def intFor(s: String) = config.getInt(s)
    Bounds(
      intFor("minRow") to intFor("maxRow"),
      intFor("minColumn") to intFor("maxColumn")
    )
