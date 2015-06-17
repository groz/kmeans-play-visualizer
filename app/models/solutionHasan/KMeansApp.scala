package models.solutionHasan

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
