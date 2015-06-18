package models.solutionRasul

import models.Clusterer
import models.Clusterer._

import scala.util.Random
// https://en.wikipedia.org/wiki/K-means_clustering
class KMeansClusterer extends Clusterer {

  def clusterize(input: Cluster, clustersCount: Int): Set[Cluster] =
  {
    val means = initialize(input, clustersCount)
    process(Set(input),means)
  }

  def process(clusters: Set[Cluster], means: Seq[V]): Set[Cluster] =
  {
    val newClusters = assignment(clusters,means)
    if (newClusters != clusters)
    {
      val newMeans = update(newClusters)
      process(newClusters,newMeans)
    } else newClusters
  }

  /** *
    * Produces initial means
    * @param input
    * @param clustersCount
    * @return
    */
  def initialize(input: Cluster, clustersCount: Int): Seq[V] =
  {
    Random.shuffle(input).take(clustersCount)
  }

  def assignment(input: Set[Cluster], means: Seq[V]): Set[Cluster] =
  {
    input.toSeq.flatten
      .groupBy(v => means.minBy(distance(_,v)))
      .mapValues(_.toVector)
      .values
      .toSet
  }

  def update(clusters: Set[Cluster]): Seq[V] =
  {
    clusters.map(c => div(c.reduce(sumOf), c.length)).toSeq
  }


  def distance(v1: V, v2: V): Double = v1.zip(v2).map(x=>Math.pow(x._1-x._2,2)).sum

  def sumOf(v1: V, v2: V): V = v1.zip(v2).map({case(x1,x2)=>x1+x2})

  def div(v: V, byValue: Double): V = v.map(_ / byValue)
}