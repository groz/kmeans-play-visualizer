package controllers

import models.solutionHasan.KMeansClusterer
import play.api.libs.json.Json
import play.api.mvc._
import play.twirl.api.Html

class Application extends Controller {

  case class Cluster(number: Int, points: Seq[Vector[Double]])
  implicit val clusterWrites = Json.writes[Cluster] // json formatter for Cluster

  val dimensions = 2

  val clusterers = Map(
    "Hasan" -> new models.solutionHasan.KMeansClusterer(),
    "Rasul" -> new models.solutionRasul.KMeansClusterer(),
    "Murad" -> new models.solutionMurad.KMeansClusterer(),
    "Hasan++" -> new models.solutionHasan.KMeansPlusPlusClusterer()
  )

  val maxClusters = 20
  val maxPoints = 1000

  def initializeField(nPoints: Int): Seq[Vector[Double]] = {
    (1 to nPoints).map(_ => (1 to dimensions).map(_ => math.random).toVector)
  }

  def kmeans(k: Int, nPoints: Int, clustererName: String) = Action {
    if (k > maxClusters || nPoints > maxPoints)
      Redirect(routes.Application.kmeans(Math.min(k, maxClusters), Math.min(nPoints, maxPoints), clustererName))
    else {
      clusterers.get(clustererName).fold(NotFound: Result) { clusterer =>
        val points = initializeField(nPoints)

        val clusters = clusterer.clusterize(points, k)

        val dataset: List[Cluster] = for {
          (cluster, idx) <- clusters.toList.zipWithIndex
        } yield Cluster(idx, cluster)

        val json = Json.toJson(dataset)

        val html = Html(Json.stringify(json))

        Ok(views.html.kmeans(dimensions, html))
      }
    }
  }

  def index = Action {
    Ok(views.html.index(clusterers.keys.toSeq))
  }
}
