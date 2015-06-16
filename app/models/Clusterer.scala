package models

import scala.util.Random

abstract class Clusterer {
  type V = Vector[Double]
  type Cluster = Seq[V]

  def clusterize(input: Cluster, k: Int): Set[Cluster]
}

// https://en.wikipedia.org/wiki/K-means_clustering
class KMeansClusterer extends Clusterer {
  def dist(v1: V, v2: V) : Double = scala.math.sqrt((v1, v2).zipped.map(_ - _).map(x => x*x).sum)
  def clusterize(input: Cluster, k: Int): Set[Cluster] = {
    //выбираю k случайных точек, которые будут центроидами
    var ncenters = Random.shuffle(input.toIndexedSeq).take(k).toVector
    var marked : Vector[(V,V)] = input.map(x => (x, ncenters.minBy(y => dist(x, y)))).toVector
    var centers : Vector[V] = ncenters

    do {
      centers = ncenters
      //помечаю весь массив своим центроидом
      marked = input.map(x => (x, centers.minBy(y => dist(x, y)))).toVector
      //вычисляю новые центры как среднее арифметическое координат точек, относящихся к центроиду
      ncenters = centers.map(x => marked.filter(y => y._2 == x).map(_._1).transpose.map(z => z.sum / z.length))
      //пока сумма расстояний между центроидами до и после не будет равна нулю
    } while (centers.zip(ncenters).map(x => dist(x._1, x._2)).sum > 0.0000001)


    marked.groupBy(x => x._2).map(y => y._2.map(z => z._1).toSeq).toSet
  }
}

object KMeansApp {
  var a = (1 to 1000).map(x => (1 to 3).map(y => math.random).toVector).toSeq
  var k = 4
  var kmeans = new KMeansClusterer()
  var clusters = kmeans.clusterize(a, k)
  println(clusters.toVector.length)
  var i = 0
  clusters.toVector.foreach(x => {
    println(s"cluster $i");
    i = i + 1;
    x.foreach(y => println(y))
  })
}