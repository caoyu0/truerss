package truerss.plugins

import java.net.URL

import com.github.truerss.ContentExtractor
import com.github.truerss.base.ContentTypeParam.{HtmlRequest, UrlRequest}
import com.github.truerss.base._
import com.typesafe.config.Config
import org.jsoup.Jsoup

import scala.collection.JavaConversions._
import scala.util.control.Exception._

class DefaultSiteReader(config: Config)
  extends BaseSitePlugin(config) {

  import org.apache.logging.log4j.LogManager
  private final val logger = LogManager.getLogger("DefaultSiteReader")

  import Errors._
  import truerss.util.Request._

  implicit def exception2error(x: Throwable) = x match {
    case x: RuntimeException => Left(ConnectionError(x.getMessage))
    case x: Exception => Left(UnexpectedError(x.getMessage))
  }

  override val author = "fntz"
  override val about = "default rss|atom reader"
  override val pluginName = "Default"
  override val version = "0.0.3"
  override val contentType = Text
  override val contentTypeParam = ContentTypeParam.URL

  override val priority = -1

  override def matchUrl(url: URL) = true

  override def newEntries(url: String) = {
    catching(classOf[Exception]) either extract(url) fold(
      err => {
        logger.error(s"new entries error -> $url", err)
        err
      },
      either => either
    )
  }

  private def extract(url: String): Either[Errors.Error, Vector[Entry]] = {
    val response = getResponse(url)

    if (response.isError) {
      Left(UnexpectedError(s"Connection error for $url with status code: ${response.code}"))
    } else {
      val x = scala.xml.XML.loadString(response.body)
      val parser = FeedParser.matchParser(x)
      val xs = parser.parse(x)

      // filter by empty url
      // transform url
      // filter description
      val result = xs.filter(_.url.isDefined)
        .map(p =>  p.copy(url = Some(normalizeLink(url, p.url.get))))
        .map { p =>
          p.description match {
            case Some(d) if d.contains("<img") =>
              p.copy(description =  Some(Jsoup.parse(d).select("img").remove().text()))
            case _ => p
          }
        }.map(_.toEntry).toVector

      Right(result)
    }
  }

  private def normalizeLink(url0: String, link: String): String = {
    val url = new URL(url0)
    val (protocol, host, port) = (url.getProtocol, url.getHost, url.getPort)

    val before = s"http"

    if (link.startsWith(before)) {
      link
    } else {
      val realPort = if (port == -1) {
        ""
      } else {
        s":$port"
      }
      s"$protocol://$host$realPort$link"
    }
  }

  override def content(urlOrContent: ContentTypeParam.RequestParam) = {
    urlOrContent match {
      case UrlRequest(url) =>
        catching(classOf[Exception]) either extractContent(url.toString) fold(
          err => {
            logger.error(s"content error -> $url", err.getMessage)
            Left(UnexpectedError(err.getMessage))
          },
          either => either
        )
      case HtmlRequest(_) => Left(UnexpectedError("Pass url only"))
    }
  }

  private def extractContent(url: String): Either[Errors.Error, Option[String]] = {
    val response = getResponse(url)
    if (response.isError) {
      Left(UnexpectedError(s"Connection error for $url"))
    } else {
      val url0 = new URL(url)
      val base = s"${url0.getProtocol}://${url0.getHost}"

      val doc = Jsoup.parse(response.body)
      val result = ContentExtractor.extract(doc.body())

      val need = doc.select(result.selector)

      need.select("img").foreach { img =>
        Option(img.absUrl("src")).map(img.attr("src", _)).getOrElse(img)
      }

      need.select("a").foreach { a =>
        val absUrl = a.attr("abs:href")
        if (absUrl.isEmpty) {
          a.attr("href", s"$base${a.attr("href")}")
        } else {
          a.attr("href", absUrl)
        }
      }

      need.select("form, input, meta, style, script").foreach(_.remove)

      Right(Some(need.html()))
    }
  }

}
