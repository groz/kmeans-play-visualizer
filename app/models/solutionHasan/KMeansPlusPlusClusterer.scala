package models.solutionHasan

import models.Clusterer

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable

// https://en.wikipedia.org/wiki/K-means_clustering
class KMeansPlusPlusClusterer extends Clusterer {
  def dist(v1: V, v2: V) : Double = scala.math.sqrt((v1, v2).zipped.map(_ - _).map(x => x*x).sum)
  def clusterize(input: Cluster, k: Int): Set[Cluster] = {
    //var ncenters = Random.shuffle(input.toIndexedSeq).take(k).toVector

    var firstCentroid = Random.shuffle(input.toIndexedSeq).toVector(0)
    var centroids : mutable.ArrayBuffer[V] = new ArrayBuffer[V](0)
    centroids += firstCentroid

    for (i <- 2 to k) {
      var withDist = input.filter(y => !centroids.exists(z => z == y)).map(x => (x, centroids.map(p => dist(x, p)).min))
      var sum = withDist.map(x => x._2).sum
      var rand = math.random * sum
      println(centroids.length)
      var d = 0.0

      var j = 0
      while (d < rand) {
        d = d + withDist(j)._2
        j = j + 1
      }
      centroids += withDist(j)._1
    }

    var ncenters : Vector[V] = centroids.toVector
    var marked : Vector[(V,V)] = input.map(x => (x, ncenters.minBy(y => dist(x, y)))).toVector
    var centers : Vector[V] = ncenters

    do {
      centers = ncenters
      marked = input.map(x => (x, centers.minBy(y => dist(x, y)))).toVector
      ncenters = centers.map(x => marked.filter(y => y._2 == x).map(_._1).transpose.map(z => z.sum / z.length))
    } while (centers.zip(ncenters).map(x => dist(x._1, x._2)).sum > 0.0000001)


    marked.groupBy(x => x._2).map(y => y._2.map(z => z._1).toSeq).toSet
  }
}
