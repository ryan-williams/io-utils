
addScala212

lazy val base =
  project
    .in(file("."))
    .settings(
      publish := {},
      test := {},
      publishArtifact := false
    )
    .aggregate(bytes, io)

lazy val bytes = project.settings(
  name := "bytes",
  version := "1.0.0",
  deps ++= Seq(
    args4j,
    args4s % "1.2.4",
    case_app,
    cats
  )
)

lazy val channel = project.settings(
  name := "channel",
  version := "1.0.0-SNAPSHOT",
  deps ++= Seq(
    hammerlab("bytes") % "1.0.0-SNAPSHOT",
    hammerlab("io") % "1.0.0-SNAPSHOT",
    math % "1.0.0-SNAPSHOT",
    paths % "1.0.0-SNAPSHOT",
    slf4j
  )
)

lazy val io = project.settings(
  name := "io",
  version := "1.0.0",
  deps ++= Seq(
    case_app,
    cats,
    paths % "1.2.0",
    slf4j
  )
)
