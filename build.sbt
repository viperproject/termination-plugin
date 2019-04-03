

//**** COPY PASTE FROM SILVER START*****
// Settings common to Silver and backends
// Compilation settings
ThisBuild / scalaVersion := "2.12.7"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",                     // Warn when using deprecated language features
  "-unchecked",                       // Warn on generated code assumptions
  "-feature",                         // Warn on features that requires explicit import
  "-Ywarn-unused-import",             // Warn on unused imports
  "-Ypatmat-exhaust-depth", "40"      // Increase depth of pattern matching analysis
)

// Publishing settings
ThisBuild / Test / publishArtifact := true
// Allows 'publishLocal' SBT command to include test artifacts in a dedicated JAR file
// (whose name is postfixed by 'test-source') and publish it in the local Ivy repository.
// This JAR file contains all classes and resources for testing and projects like Carbon
// and Silicon can rely on it to access the test suit implemented in Silver.
//**** COPY PASTE FROM SILVER END *****


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
  scalaVersion := "2.12.8",

    // Test settings
    Test / parallelExecution := false,

  // Assembly settings
  assembly / assemblyJarName := "termination.jar",
  assembly / mainClass := None,
  assembly / test := {}
)