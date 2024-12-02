import sbt.*

object Dependencies {

  object Versions {
    val aoc = "2.0.2"
    val enumeratum = "1.7.5"
  }

  object Libraries {
    val aoc = "com.pg.bigdata" % "da-ap-pda-nas-aoc-scala_3" % Versions.aoc
    val enumeratum = "com.beachape" %% "enumeratum" % Versions.enumeratum
  }
}
