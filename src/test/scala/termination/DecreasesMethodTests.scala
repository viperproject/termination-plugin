
package viper.plugin.termination

import viper.silicon.tests.SiliconTests

class DecreasesMethodTests extends SiliconTests {
  override val testDirectories: Seq[String] = Seq("termination/methods")
  override val commandLineArguments: Seq[String] = Seq("--plugin", "viper.termination.DecreasesMethod")
}
