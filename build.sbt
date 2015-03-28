name := "inferencer"

organization := "bucharestfp"

version := "0.1.0"

scalaVersion := "2.11.6"

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Xlint:_",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-value-discard"
)

libraryDependencies ++= Seq(
  "org.slf4j"       % "slf4j-api"        % "1.7.10",
  "org.slf4j"       % "jcl-over-slf4j"   % "1.7.10" % "test",
  "org.slf4j"       % "jul-to-slf4j"     % "1.7.10" % "test",
  "org.slf4j"       % "log4j-over-slf4j" % "1.7.10" % "test",
  "ch.qos.logback"  % "logback-classic"  % "1.1.2"  % "test",
  "ch.qos.logback"  % "logback-core"     % "1.1.2"  % "test",
  "org.scalatest"  %% "scalatest"        % "2.2.4"  % "test"
)

testOptions ++= Seq(
  // Avoid "substitute logger" warning from SLF4J when running tests in
  // parallel. See also: stackoverflow.com/questions/7898273#12095245.
  Tests.Setup { cl =>
    cl.loadClass("org.slf4j.LoggerFactory")
      .getMethod("getLogger", cl.loadClass("java.lang.String"))
      .invoke(null, "ROOT")
  }
)

graphSettings
