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
  version := "0.1",
  scalaVersion := "2.12.8",

  // Assembly settings
  assembly / assemblyJarName := "termination.jar",
  assembly / mainClass := None,
  assembly / test := {}
)