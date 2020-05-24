import mill._, scalalib._

object racy extends ScalaModule {
  def scalaVersion = "2.13.1"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:0.2.0"
  )
}
