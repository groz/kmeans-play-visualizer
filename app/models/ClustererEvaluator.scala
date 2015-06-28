package models

import models.Clusterer.Cluster

object ClustererEvaluator {
  import FotmMath._

  def calcCentroid(cluster: Cluster) = div(cluster.reduce(add), cluster.length)

  def evaluateClusterer(clusterer: Clusterer, points: Seq[Clusterer.V], k: Int) = {
    val result = clusterer.clusterize(points, k)
    evaluateResult(result)
  }

  def evaluateResult(clusters: Set[Cluster]) = {
    val nPoints: Int = clusters.map(_.length).sum
    val clustersWithCentroids = clusters.map(c => (c, calcCentroid(c)))

    // sum deviations of all points to the centroid of appropriate cluster
    val distances: Set[Double] = for {
      (cluster, centroid) <- clustersWithCentroids
      point <- cluster.toSeq
    } yield dist(point, centroid)

    distances.sum / nPoints
  }

  def uniformPoints = uniformlyDistributedPoints(300)
  def gaussianPoints = gaussianDistributedPoints(300, 10)
}



object ClustererEvaluatorApp extends App {
  import ClustererEvaluator._

  val results = for {
    (name, clusterer) <- Clusterer.implementations.toList
  } yield {
      val results = for {
        i <- 1 to 5
        k <- 5 to 40
      } yield evaluateClusterer(clusterer(), gaussianPoints, k)

      (name, results.sum / results.length)
    }

  results.foreach { r =>
    val (name, result) = r
    println(s"$name: $result")
  }
}
