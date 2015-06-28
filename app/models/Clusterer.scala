package models

import scala.util.Random

object Clusterer {
  type V = Vector[Double]
  type Cluster = Seq[V]

  lazy val implementations = Map(
    "Hasan" -> { () => new models.solutionHasan.KMeansClusterer() },
    "Rasul" -> { () => new models.solutionRasul.KMeansClustererSimple() },
    "Murad" -> { () => new models.solutionMurad.KMeansClusterer() },
    "HasanFP++" -> { () => new models.solutionHasan.FpPlusPlusKMeansClusterer() },
    "Timur" -> { () => new models.solutionTimur.KMeansClusterer() }
  )

}

import models.Clusterer._

trait Clusterer {
  type V = Vector[Double]
  type Cluster = Seq[V]
  def clusterize(input: Cluster, groupSize: Int): Set[Cluster]
}

