package truerss.plugins

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import truerss.util.{PreEntry, Util}

import scala.util.Try
import scala.xml.{Elem, Node}

trait FeedParser {
  import Util._

  def from(tagName: String)(implicit source: Node): Option[String] = {
    val x = source \ tagName
    if (x.isEmpty) { None }
    else { Some(x.text) }
  }
  def parse(x: Elem): Iterable[PreEntry]

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

  override def parse(x: Elem): Iterable[PreEntry] = {
    (x \\ _item).map { implicit item =>
      PreEntry(
        title = from(_title),
        url = from(_link),
        description = from(_description),
        publishedDate = from(_pubDate).flatMap(getDate).getOrElse(new java.util.Date),
        author = from(_author)
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
    val links = x \ _link
    val r = links
      .filter(_.attribute("rel").exists(_.forall(_.text == "alternate")))
      .flatMap(_.attribute("href").map(_.text)).headOption

    if (links.nonEmpty && r.isEmpty) {
      links.headOption.flatMap(_.attribute("href").map(_.text))
    } else {
      r
    }
  }

  implicit val format = DateTimeFormatter.ISO_DATE_TIME

  override def parse(x: Elem): Iterable[PreEntry] = {
    // global author
    val xs = x \ _author
    val g = if (xs.nonEmpty) {
      (xs \ _name).headOption.map(_.text).orElse(Some(xs.text))
    } else {
      None
    }

    (x \ _entry).map { implicit entry =>
      PreEntry(
        url = getLinks(entry),
        title = from(_title),
        author = getAuthors(entry).orElse(g),
        publishedDate = from(_updated).flatMap(getDate).getOrElse(new java.util.Date),
        description = from(_summary)
      )
    }
  }
}