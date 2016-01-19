package truerss.controllers

import java.io.StringReader
import java.net.URL

import akka.pattern.ask
import com.github.fntzr.spray.routing.ext.BaseController
import com.rometools.opml.feed.opml.Opml
import com.rometools.rome.io.WireFeedInput
import org.xml.sax.InputSource
import spray.http.{HttpCharsets, HttpCharset, MultipartFormData, StatusCodes}
import spray.routing.{Route, HttpService}
import truerss.models.{ApiJsonProtocol, Source, SourceHelper}
import truerss.system.{db, util}
import truerss.util.Lens
import truerss.util.OpmlParser

import scala.concurrent.Future
import scala.util.control.Exception._
import scala.util.{Failure => F, Success => S, Try}

import shapeless.syntax.typeable._

trait SourceController extends BaseController
  with ProxyRefProvider with ActorRefExt with ResponseHelper {

  import ApiJsonProtocol._
  import HttpService._
  import context.dispatcher
  import db._
  import spray.json._
  import util._
  import Lens._

  import scala.collection.JavaConversions._

  def all = end(GetAll)

  def show(num: Long) = end(GetSource(num))

  private def addOrUpdate(s: String)(f: Source => Route) = {
    catching(classOf[Exception])
      .opt(JsonParser(s).convertTo[Source]) match {
      case Some(fs) => f(fs)
      case None => complete(spray.http.StatusCodes.BadRequest, "Not valid data")
    }
  }

  def create = entity(as[String]) { sourceString =>
    addOrUpdate(sourceString) { source => end(AddSource(source.normalize)) }
  }

  def update(num: Long) = entity(as[String]) { sourceString =>
    addOrUpdate(sourceString) { source =>
      end(UpdateSource(num, source.normalize.newId(num)))
    }
  }

  def delete(num: Long) = end(DeleteSource(num))

  def markAll(num: Long) = end(MarkAll(num))

  def latest(count: Long) = end(Latest(count))

  def feeds(num: Long) = {
    parameters('from ? "0", 'limit ? "100") { (from, limit) =>
      end(ExtractFeedsForSource(num, ttry(from, 0), ttry(limit, 100)))
    }
  }
  def refresh = end(Update)

  def refreshOne(num: Long) = end(UpdateOne(num))

  def unread(sourceId: Long) = end(Unread(sourceId))

  def fromFile = {
    entity(as[MultipartFormData]) { formData => c =>
      val interval = 8
      val file = formData.fields.map(_.entity.asString(HttpCharsets.`UTF-8`))
        .reduce(_ + _)
      OpmlParser.parse(file).fold(
        err => {
          proxyRef ! Notify(NotifyLevels.Danger, s"Error when import file $err")
          c.complete(StatusCodes.BadRequest, err)
        },
        xs => {
          val result = xs.map { x =>
            SourceHelper.from(x.link, x.title, interval)
          }.map(s => (proxyRef ? AddSource(s.normalize)).mapTo[Response])
          Future.sequence(result).onComplete {
            case S(xs) =>
              xs.foreach {
                case BadRequestResponse(msg) =>
                  proxyRef ! Notify(NotifyLevels.Danger, msg)
                case _ =>
              }
              c.complete(StatusCodes.OK, "ok")
            case F(error) =>
              proxyRef ! Notify(NotifyLevels.Danger, s"Error when import file ${error.getMessage}")
              c.complete(StatusCodes.BadRequest, "oops")
          }
        }
      )
    }
  }

  private def ttry(possibleInt: String, recover: Int) =
    Try(possibleInt.toInt).getOrElse(recover)

}
