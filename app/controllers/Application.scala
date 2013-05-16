package controllers

import play.api._
import play.api.mvc._
import play.api.http.HeaderNames
import play.api.libs.ws.{WS, Response}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future

object Application extends Controller {

  def about = Action {
    Ok(views.html.about())
  }

  def hayooFu(q: String): Future[Response] = {
    import java.net.URLEncoder
    WS.url("http://holumbus.fh-wedel.de/hayoo/hayoo.json?query=" +
      URLEncoder.encode("name:\"" + q + "\"", "utf-8")).withTimeout(10 * 1000).
      get
  }

  def query(q: String) = Action {
    Async {
      import scala.math.Ordering
      hayooFu(q).map { resp: Response =>
        val pkgs = parseJson(resp.json).fold(_ => List.empty[Func],
          identity[List[Func]]).sorted(Ordering.by[Func, String](_.pkg.toLowerCase))
        Ok(views.html.result(q, pkgs))
      }
    }
  }


  import scala.io._
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  final case class Func(name: String, uri: String, module: String,
                        sig: String, pkg: String, desc: String)

  object Func extends Reads[Func] {
    /* WRONG, apply causes overloading ambiguity
    implicit def apply(): Reads[Func] = (
      (__ \ "name").read[String] and
        (__ \ "uri").read[String] and
        (__ \ "module").read[String] and
        (__ \ "signature").read[String] and
        (__ \ "package").read[String] and
        (__ \ "description").read[String]
      )(Func.apply _)
      */
    implicit val funcReads: Reads[Func] = (
      (__ \ "name").read[String] and
        (__ \ "uri").read[String] and
        (__ \ "module").read[String] and
        (__ \ "signature").read[String] and
        (__ \ "package").read[String] and
        (__ \ "description").read[String]
      )(Func.apply _)
    def reads(json: JsValue): JsResult[Func] = json.validate
  }

  def parseJson(value: JsValue): JsResult[List[Func]] = {
    implicit val flistReads: Reads[List[Func]] = (
      (__ \ "functions").read[List[Func]](Reads.list[Func]))

    value.validate[List[Func]]
  }

  def parseJsonPkg(value: JsValue): JsResult[Seq[String]] = {

    case class Pkg(name: String, count: Int)
    implicit val pkgReads: Reads[Pkg] = (
      (__ \ "name").read[String] and // see FunctionalBuilder for 'and'
        (__ \ "count").read[Int]
      )(Pkg)

    // FunctionalBuilder[Reads]#CanBuild2[String, Int]
    // val x = (__ \ "name").read[String] and (__ \ "count").read[Int]

    implicit val pkgsReads = (__ \ "packages").read[JsArray]

    value.validate[JsArray].map { case JsArray(sq) =>
      // JsArray(Seq[JsValue]) => Seq[String]
      (sq :\ Seq.empty[String]) { (jv, s) =>
        jv.validate[Pkg].fold(_ => s, p => p.name +: s)
      }
    }

  }

  def parseJsonPkg2: JsResult[Seq[String]] = {
    import java.io.File
    val dummy = Source.fromFile("/tmp/hayoo.json")(Codec.UTF8)
    val value = Json.parse(dummy.mkString) // : JsValue
    parseJsonPkg(value)
  }
}