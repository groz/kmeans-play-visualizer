package models.solutionRasul

object MainApp extends App
{
  var clusterer = new KMeansClusterer
  type V = Vector[Double]
  type Cluster = Seq[V]

  val cluster: Cluster =
    Seq(
      Vector(1,1),
      Vector(2,2),
      Vector(10,10),
      Vector(11,11),
      Vector(25,25),
      Vector(26,26)
    )

  val clusters = clusterer.clusterize(cluster,3)
  println(clusters)
}