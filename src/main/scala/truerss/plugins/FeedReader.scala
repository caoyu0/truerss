package truerss.plugins

import java.time.{LocalDateTime, LocalDate}
import java.time.format.DateTimeFormatter
import java.util.Date

import scala.util.Try

import com.github.truerss.base.Entry
import truerss.util.Util

import scala.xml.{Elem, Node}

trait FeedParser {
  import Util._

  def from(tagName: String)(implicit source: Node): Option[String] = {
    val x = source \ tagName
    if (x.isEmpty) { None }
    else { Some(x.text) }
  }
  def parse(x: Elem): Iterable[Entry]

  def getDate(x: String)(implicit format: DateTimeFormatter) = {
    Try(LocalDateTime.parse(x.toCharArray, format)).toOption.map(_.toDate)
  }
}

case object FeedParser {
  def matchParser(x: Elem) = {
    if (x.label == "RDF" || x.label == "rss") {
      RSSParser
    } else {
      AtomParser
    }
  }
}

case object RSSParser extends FeedParser {
  val _title = "title"
  val _link = "link"
  val _description = "description"
  val _item = "item"
  val _pubDate = "pubDate"
  val _author = "author"

  implicit val format = DateTimeFormatter.RFC_1123_DATE_TIME

  override def parse(x: Elem): Iterable[Entry] = {
    (x \\ _item).map { implicit item =>
      Entry(
        title = from(_title).get,
        url = from(_link).get,
        description = from(_description),
        publishedDate = from(_pubDate).flatMap(getDate).getOrElse(new java.util.Date),
        author = from(_author).getOrElse(""),
        content = None
      )
    }
  }
}

case object AtomParser extends FeedParser {
  val _author = "author"
  val _name = "name"
  val _link = "link"
  val _updated = "updated"
  val _title = "title"
  val _entry = "entry"
  val _summary = "summary"

  protected def getAuthors(x: Node) = {
    val r = x \ _author
    if (r.isEmpty) {
      None
    } else {
      Some(r.flatMap(e => (e \ _name).map(_.text)).mkString(", "))
    }
  }

  protected def getLinks(x: Node) = {
    (x \ _link)
      .filter(_.attribute("rel").exists(_.forall(_.text == "alternate")))
      .flatMap(_.attribute("href").map(_.text)).headOption
  }

  implicit val format = DateTimeFormatter.ISO_DATE_TIME

  override def parse(x: Elem): Iterable[Entry] = {
    (x \ _entry).map { implicit entry =>
      Entry(
        url = getLinks(entry).get,
        title = from(_title).get,
        author = getAuthors(entry).getOrElse(""),
        publishedDate = from(_updated).flatMap(getDate).getOrElse(new java.util.Date),
        description = from(_summary),
        content = None
      )
    }
  }
}