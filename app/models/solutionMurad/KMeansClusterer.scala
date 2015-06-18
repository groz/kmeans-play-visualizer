package models.solutionMurad

import models.Clusterer
import models.Clusterer._

// https://en.wikipedia.org/wiki/K-means_clustering
class KMeansClusterer extends Clusterer {

  var mainCluster: Cluster = Seq.empty[V]

  def randomVector(dotCount: Int, dotRange: Double): V = (1 to dotCount).map(x => dotRange*math.random).toVector;
  def getRandomVectors(count: Int, dotCount: Int, dotRange: Double): Cluster = (1 to count).map(x => randomVector(dotCount, dotRange)).toSeq

  def distance(x: V, y: V):Double = x.diff(y).map(a => a*a).sum;
  def closestBasicIndex(vec: V, base: Seq[V]): Int = base.indexOf(base.minBy(x => distance(x, vec)));
  def closestVector(vec: V, base: Seq[V]): V = base.minBy(x => distance(x, vec));

  def getClustersByBase(input: Cluster, basicVecs: Seq[V]): Set[Cluster] =
    input.groupBy(vec => closestVector(vec, basicVecs)).values.toSet

  def vectorSum(v1: V, v2: V): V = (1 to v1.length).map(i => v1.apply(i)+v2.apply(i)).toVector;
  def vectorDiv(v: V,n: Int): V = (1 to v.length).map(i => v.apply(i) / n).toVector
  
  def meanVectorAll(cluster: Cluster):V = vectorDiv(cluster.foldLeft(Vector.empty[Double])(vectorSum(_,_)), cluster.length);
  def meanVector(cluster: Cluster):V = closestVector(meanVectorAll(cluster), cluster)

  def getMeanVectors(clusterSet: Set[Cluster]):Seq[V] =
    clusterSet.map(cluster => meanVector(cluster)).toSeq

  def recursiveFunc(clusterSet: Set[Cluster]): Set[Cluster] = {
    val curMove = getClustersByBase(mainCluster, getMeanVectors(clusterSet));
    if (curMove == clusterSet) curMove
    else  recursiveFunc(curMove)
  }


  def clusterize(input: Cluster, n: Int): Set[Cluster] = {

    mainCluster = input;
    recursiveFunc(getClustersByBase(input, input.take(n)))

  }
}

object MainApp extends App {

  def randomVector(dimens: Int, dRange: Double): Vector[Double] = (1 to dimens).map(x => dRange*math.random).toVector;
  def getRandomVectors(count: Int, dimens: Int, dRange: Double): Seq[Vector[Double]] = (1 to count).map(x => randomVector(dimens, dRange)).toSeq

  val dotCount: Int = 20;
  val dimensn: Int = 5;
  val range: Double = 15

  val Vecs = getRandomVectors(dotCount, dimensn, range);
  val n = 4

  println(s"Vectors are: \n ${Vecs}. Count of clusters is: $n")

  val clusters = new KMeansClusterer

  clusters.clusterize(Vecs, n)

  println(s"Vectors are: \n ${clusters}")
}

