
package viper.plugin.termination

import viper.silicon.tests.SiliconTests

class Test extends SiliconTests {
  override val testDirectories: Seq[String] = Seq("termination/decreases/unfolding")
  override val commandLineArguments: Seq[String] = Seq("--plugin", "viper.termination.DecreasesSimple")
}
