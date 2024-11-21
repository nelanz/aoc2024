import sbt.*

object Dependencies {

  object Versions {
    val aoc = "2.0.0"
  }

  object Libraries {
    val aoc = "com.pg.bigdata" %% "da-ap-pda-nas-aoc-scala_3" % Versions.aoc
  }
}
