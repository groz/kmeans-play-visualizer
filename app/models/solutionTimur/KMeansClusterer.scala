package models.solutionTimur

import models.Clusterer

class KMeansClusterer extends Clusterer {
  def average(x: Seq[Double]): Double = x.sum / x.length

  def dist(v1: V, v2: V): Double  = v1.zip(v2).map(x => math.pow(x._1 - x._2, 2)).sum

  def newcenter(vectors: Cluster): V = vectors.transpose.map(average).toVector

  def redivide(input: Cluster, newcenters: Seq[V]): Seq[Cluster] =
  {
    val pairs = input.map(x => (x, newcenters.minBy(y => dist(y, x))))
    pairs.groupBy(x => x._2).map(z => z._2.map(y => y._1)).toSeq
  }

  def clusterizeRecursion(centers: Seq[V], input: Cluster, k: Int): Seq[Cluster] =
  {
    val clusters = redivide(input, centers)
    val newcenters = clusters.map(newcenter)
    if (newcenters == centers) clusters
    else clusterizeRecursion(newcenters, input, k)
  }

  def clusterize(input: Cluster, k: Int): Set[Cluster] =
    clusterizeRecursion(scala.util.Random.shuffle(input).take(k), input, k).toSet
}