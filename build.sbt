val scala3Version = "3.7.1"

lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := scala3Version)

lazy val lince20 = project.in(file("."))
   .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "Lince 2.0",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("lince.frontend.Main"),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "lib" / "caos"/ "tool" / "js" / "gen",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % "1.1.0",  //"0.3.4",  // parser combinators
      "org.scalameta" %% "munit" % "1.1.1" % Test  //"0.7.29" % Test // unit tests
    )
  )
  .dependsOn(caos)