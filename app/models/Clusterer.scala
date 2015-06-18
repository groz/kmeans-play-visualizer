package models

import scala.util.Random

object Clusterer {
  type V = Vector[Double]
  type Cluster = Seq[V]

  lazy val implementations = Map(
    "Hasan" -> { () => new models.solutionHasan.KMeansClusterer() },
    "Rasul" -> { () => new models.solutionRasul.KMeansClusterer() },
    "Murad" -> { () => new models.solutionMurad.KMeansClusterer() },
    "HasanFP++" -> { () => new models.solutionHasan.FpPlusPlusKMeansClusterer() }
    //"Hasan++" -> { () => new models.solutionHasan.KMeansPlusPlusClusterer() }
  )

}

import models.Clusterer._

abstract class Clusterer {
  def clusterize(input: Cluster, k: Int): Set[Cluster]
}

object FotmMath {
  def add(v1: V, v2: V) = v1.zip(v2).map(v => v._1 + v._2)

  def sub(v1: V, v2: V) = add(v1, v2.map(- _))

  def dist(v1: V, v2: V) = len(sub(v1, v2))

  def sqrdist(v1: V, v2: V) = sqrlen(sub(v1, v2))

  def div(v: V, a: Double) = v.map(_ / a)

  def scale(v: V, a: Double) = v.map(_ * a)

  def len(v: V) = Math.sqrt( sqrlen(v) )

  def sqrlen(v: V) = v.map(a => a * a).sum

  def uniformlyDistributedPoints(nPoints: Int): Seq[Clusterer.V] = {
    val rng = new Random()
    (1 to nPoints).map { i => Vector(rng.nextDouble, rng.nextDouble) }
  }

  def gaussianDistributedPoints(nPoints: Int, nSeeds: Int): Seq[Clusterer.V] = {
    val rng = new Random()
    val seeds = (1 to nSeeds).map { i => Vector(rng.nextDouble, rng.nextDouble) }
    for {
      s <- seeds
      groupSize <- (1 to nPoints / nSeeds)
    } yield scale(add(s, Vector(rng.nextGaussian * .05, rng.nextGaussian * .05)), 0.8)
  }
}

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
