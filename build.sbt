// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import java.nio.file.Paths

import sbt.Keys.organization

import scala.sys.process._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}


enablePlugins(ScalaJSPlugin)

name := "html-app"
version := "0.1.2-SNAPSHOT"
organization := "bon.jo"
scalaVersion := "3.0.0" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
scalacOptions ++= Seq("-deprecation", "-feature")
javacOptions ++= Seq("-encoding", "UTF-8")
libraryDependencies ++=Seq(( "org.scala-js" %%% "scalajs-dom" % "1.0.0").cross(CrossVersion.for3Use2_13) ,"org.scala-lang.modules" %%% "scala-xml" % "2.0.0",
  "bon.jo" %%% "phy-shared" % "1.0.0-SNAPSHOT"
)




lazy val chromePath =  Paths.get("""C:\Program Files (x86)\Google\Chrome\Application\chrome.exe""")
lazy val chrome = taskKey[Unit]("open fast in chrome")

chrome := {
  s""""$chromePath" "${baseDirectory.value.toPath}/test.html""".!
}