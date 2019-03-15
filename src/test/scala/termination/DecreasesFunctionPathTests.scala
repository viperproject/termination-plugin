/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.termination

import viper.silicon.tests.SiliconTests

class DecreasesFunctionPathTests extends SiliconTests {
  override val testDirectories: Seq[String] = Seq("termination/functions/decreases", "termination/functions/decreases_path")

  override val commandLineArguments: Seq[String] = Seq("--plugin", "viper.termination.DecreasesFunctionPath")
}
