package truerss.util

import scala.xml._
import scala.util.control.Exception._
import scala.util.Either

case class Outline(title: String, link: String)

object OpmlParser {

  val _outline = "outline"
  val _title = "title"
  val _xmlUrl = "xmlUrl"

  def parse(s: String): Either[String, Iterable[Outline]] = {
    catching(classOf[Exception]) either load(s) fold(
      err => Left(err.getMessage),
      xs => Right(xs)
    )
  }

  private def present(attr: String)(implicit node: Node): Boolean = {
    val r = node.attribute(attr)
    r.isDefined && r.forall(_.nonEmpty)
  }

  private def get(attr: String)(implicit node: Node): String = {
    node.attribute(attr).map(_.text).head
  }

  private def load(s: String) = {
    val x = XML.loadString(s)
    (x \\ _outline).filter { implicit outline =>
      present(_title) && present(_xmlUrl)
    }.map { implicit outline =>
      val z = Outline(get(_title), get(_xmlUrl))
      println(z)
      z
    }
  }

}
