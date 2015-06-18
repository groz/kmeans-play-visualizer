package models

import models.Clusterer._

object Clusterer {
  type V = Vector[Double]
  type Cluster = Seq[V]
}

abstract class Clusterer {
  def clusterize(input: Cluster, k: Int): Set[Cluster]
}

object FotmMath {
  def add(v1: V, v2: V) = v1.zip(v2).map(v => v._1 + v._2)

  def sub(v1: V, v2: V) = add(v1, v2.map(- _))

  def dist(v1: V, v2: V) = len(sub(v1, v2))

  def div(v: V, a: Double) = v.map(_ / a)

  def len(v: V) = Math.sqrt( sqrlen(v) )

  def sqrlen(v: V) = v.map(a => a * a).sum
}

object ClustererEvaluatorApp extends App {
  import FotmMath._

  def calcCentroid(cluster: Cluster) = div(cluster.reduce(add), cluster.length)

  def evaluate(clusters: Set[Cluster]) = {
    val nClusters: Int = clusters.map(_.length).sum
    val clustersWithCentroids = clusters.map(c => (c, calcCentroid(c)))

    // sum deviations of all points to the centroid of appropriate cluster
    val distances: Set[Double] = for {
      (cluster, centroid) <- clustersWithCentroids
      point <- cluster.toSeq
    } yield dist(point, centroid)

    distances.sum
  }

}