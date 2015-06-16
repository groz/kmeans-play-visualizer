package controllers

import models.KMeansClusterer
import play.api.libs.json.Json
import play.api.mvc._
import play.twirl.api.Html

class Application extends Controller {

  case class Cluster(number: Int, points: Seq[Vector[Double]])
  implicit val clusterWrites = Json.writes[Cluster] // json formatter for Cluster

  val dimensions = 2
  val clusterer = new KMeansClusterer()

  val maxClusters = 20
  val maxPoints = 1000

  def kmeans(k: Int, nPoints: Int, clustererName: String) = Action {
    if (k > maxClusters || nPoints > maxPoints) {
      Redirect(routes.Application.kmeans(Math.min(k, maxClusters), Math.min(nPoints, maxPoints), clustererName))
    } else {
      val points =
        (1 to maxPoints)
          .map(_ => (1 to dimensions).map(_ => math.random).toVector)

      val clusters = clusterer.clusterize(points, k)

      val dataset: List[Cluster] = for {
        (cluster, idx) <- clusters.toList.zipWithIndex
      } yield Cluster(idx, cluster)

      val json = Json.toJson(dataset)

      val html = Html(Json.stringify(json))

      Ok(views.html.kmeans(dimensions, html))
    }
  }

  def index = Action {
    Ok(views.html.index())
  }
}
