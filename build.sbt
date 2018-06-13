lazy val akkaHttpVersion = "10.1.1"
lazy val akkaVersion = "2.5.12"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "science.snelgrove",
      scalaVersion := "2.12.5"
    )),
    name := "showdown-cli",
    connectInput in run := true,
    outputStrategy := Some(StdoutOutput),
    fork in run := true,
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.chuusai" %% "shapeless" % "2.3.2",
      "com.googlecode.lanterna" % "lanterna" % "3.0.1",
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-xml" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.play" %% "play-json" % "2.6.7",
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalaz" %% "scalaz-core" % "7.2.15",

      "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
      "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )
