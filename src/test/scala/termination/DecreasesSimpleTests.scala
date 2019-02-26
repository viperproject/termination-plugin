/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package termination

class DecreasesSimpleTests extends SiliconTests {
  override val testDirectories: Seq[String] = Seq("termination/decreases", "termination/decreases_path")
  override val commandLineArguments: Seq[String] = Seq("--plugin", "viper.silver.plugin.termination.DecreasesSimple")
}
