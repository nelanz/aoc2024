import scala.collection.Seq

ThisBuild / useCoursier := false
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.1"

lazy val root = (project in file("."))
  .enablePlugins(ScalafmtPlugin, BuildInfoPlugin)
  .settings(
    name := "aoc2024",
    credentials ++= Seq(Credentials(Path.userHome / ".sbt" / ".credentials"), Credentials(Path.userHome / "credentials.txt")),
    resolvers ++= Seq("utils" at "https://pkgs.dev.azure.com/dh-platforms-devops/app-deng-nas_us/_packaging/com.pg.bigdata/maven/v1"),
    libraryDependencies ++= Seq(
      Dependencies.Libraries.aoc,
      Dependencies.Libraries.enumeratum
    )
  )
