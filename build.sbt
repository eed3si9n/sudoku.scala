name := "sudoku.scala"

organization := "com.eed3si9n"

homepage := Some(url("http://eed3si9n.com/"))

scalaVersion := "2.10.0-RC2"

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-typelevel" % "7.0.0-M4" cross CrossVersion.full
)

resolvers ++= Seq(
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public"
)

initialCommands in console := """import scalaz._, Scalaz._, typelevel._
                                |import com.eed3si9n.sudoku._
                                |val game = com.eed3si9n.sudoku.Reader.read("data/3.sdk")""".stripMargin
