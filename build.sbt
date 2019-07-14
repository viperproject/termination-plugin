// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

// Import general settings from Silver
lazy val silver = project in file("silver")

// Import general settings from Silver
lazy val silicon = project in file("silicon")

// Silicon specific project settings
lazy val term = (project in file("."))
  .dependsOn(silver % "compile->compile;test->test")
  .dependsOn(silicon % "test->test")
  .settings(
    name := "termination-plugin",
    organization := "viper",
    version := "0.1",

    // Run settings
    run / javaOptions += "-Xss128m",

    // Test settings
    Test / javaOptions ++= (run / javaOptions).value,
    fork := true, // See Silicon's build.sbt for details
      
    // Assembly settings
    assembly / assemblyJarName := "termination.jar",
    assembly / mainClass := None,
    assembly / test := {}
  )
