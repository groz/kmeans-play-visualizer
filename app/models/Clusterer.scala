package models

abstract class Clusterer {
  type V = Vector[Double]
  type Cluster = Seq[V]

  def clusterize(input: Cluster, k: Int): Set[Cluster]
}