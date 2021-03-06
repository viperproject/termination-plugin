/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.plugin.termination

import viper.silicon.tests.SiliconTests

class DecreasesMethodTests extends SiliconTests {
  override val testDirectories: Seq[String] = Seq("termination/methods")
  override val commandLineArguments: Seq[String] = Seq("--plugin", "viper.plugin.termination.DecreasesMethod")
}
