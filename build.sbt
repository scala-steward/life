val scala3Version = "3.2.0"

organizationName := "Nigel Eke"
startYear        := Some(2022)
licenses += ("BSD-3-Clause", new URL("https://opensource.org/licenses/BSD-3-Clause"))

val configVersion        = "1.4.2"
val scalatestVersion     = "3.2.14"
val scalatestPlusVersion = scalatestVersion + ".0"

Compile / compile / wartremoverErrors ++= Warts.allBut(Wart.Equals, Wart.Recursion, Wart.ThreadSleep)

lazy val root = project
  .in(file("."))
  .settings(
    name         := "life",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.typesafe"       % "config"          % configVersion,
      "org.scalactic"     %% "scalactic"       % scalatestVersion,
      "org.scalatest"     %% "scalatest"       % scalatestVersion     % "test",
      "org.scalatestplus" %% "scalacheck-1-17" % scalatestPlusVersion % "test"
    )
  )
