package models.solutionHasan

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable

import models.Clusterer
import models.Clusterer._

// https://en.wikipedia.org/wiki/K-means_clustering
class FpPlusPlusKMeansClusterer extends Clusterer {
  def dist(v1: V, v2: V) : Double = scala.math.sqrt((v1, v2).zipped.map(_ - _).map(x => x*x).sum)
  def clusterize(input: Cluster, k: Int): Set[Cluster] = {
    def findRandomEl (inputElements : Vector[(V, Double)], rand: Double, sum: Double, id: Int, acc: Double) : V = {
      if (id == inputElements.length - 1 || acc >= rand)inputElements(id)._1 else {
        findRandomEl(inputElements, rand, sum, id + 1, acc + inputElements(id)._2)
      }
    }

    def GetCentroids (inputPoints: Vector[V], existingCentroids : Vector[V], count : Int): Vector[V] = {
      if (count == 0) existingCentroids else  {
        if (!existingCentroids.isEmpty) {
        val withDist = input.filter(y => !existingCentroids.exists(z => z == y)).map(x => (x, existingCentroids.map(p => dist(x, p)).min)).toVector
        val sum = withDist.map(x => x._2).sum
        val rand = math.random * sum
        val re = findRandomEl(withDist, rand, sum, 0, 0)
        GetCentroids(inputPoints, existingCentroids :+ re, count - 1)
        } else {
          val re = Random.shuffle(input.toIndexedSeq).toVector(0)
          GetCentroids(inputPoints, existingCentroids :+ re, count - 1)
        }
      }
    }

    val centers : Vector[V] = GetCentroids(input.toVector, Vector(), k)

    def GetResultR (centers: Vector[V], input: Cluster): Set[Cluster] = {
      val marked = input.map(x => (x, centers.minBy(y => dist(x, y)))).toVector
      val ncenters = centers.map(x => marked.filter(y => y._2 == x).map(_._1).transpose.map(z => z.sum / z.length))
      if (centers.zip(ncenters).map(x => dist(x._1, x._2)).sum > 0.0000001)
        GetResultR(ncenters, input)
      else
        marked.groupBy(x => x._2).map(y => y._2.map(z => z._1).toSeq).toSet
    }

    GetResultR(centers, input)
  }
}
