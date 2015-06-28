import models.Clusterer.{V, Cluster}

def dist(a: V, b: V) = a.zip(b).map{ (ab) =>
  val (x, y) = ab
  (x - y) * (x - y)
}.sum

def redivide(input: Cluster, centers: Seq[V]): Seq[Cluster] = {
  // map from input point to closest center
  val lookup: Map[V, V] = input.map(v => (v, centers.minBy(dist(_, v)))).toMap
  // group input points by closest center
  val grouped: Map[V, Seq[V]] = input.groupBy(lookup)
  // each value of the map is a cluster
  grouped.values.toSeq
}

object MyCollection extends Traversable[Int] {
  def foreach[U](f: (Int) => U): Unit = f(5)
}

MyCollection.map(_ * 5)