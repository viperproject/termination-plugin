/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.plugin.termination

import viper.silicon.tests.SiliconTests

class DecreasesFunctionTests extends SiliconTests {
  override val testDirectories: Seq[String] = Seq("termination/functions/decreases")
  override val commandLineArguments: Seq[String] = Seq("--plugin", "viper.plugin.termination.DecreasesFunction")
}
