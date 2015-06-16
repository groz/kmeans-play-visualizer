package controllers

import models.KMeansClusterer
import play.api.libs.json.{JsValue, Json, JsPath, Writes}
import play.api.mvc._
import play.api.libs.functional.syntax._
import play.twirl.api.Html

class Application extends Controller {

  val clusterer = new KMeansClusterer()

  case class Cluster(number: Int, points: List[Vector[Double]])

  def index = Action {

    val dimensions = 2
    val a = (1 to 300).map(x => (1 to dimensions).map(y => math.random).toVector).toSeq
    val k = 9
    val clusters: Set[clusterer.Cluster] = clusterer.clusterize(a, k)

    val dataset: List[Cluster] = for {
      (cluster, idx) <- clusters.toList.zipWithIndex
    } yield Cluster(idx, cluster.toList)

    implicit val locationWrites: Writes[Cluster] = (
      (JsPath \ "number").write[Int] and
      (JsPath \ "points").write[List[Vector[Double]]]
    )(unlift(Cluster.unapply))

    val json: JsValue = Json.toJson(dataset)

    val html = Html(Json.stringify(json))

    Ok(views.html.index(dimensions, html))
  }
}
