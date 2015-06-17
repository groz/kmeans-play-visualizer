package models.solutionHasan

import models.Clusterer

import scala.util.Random

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
