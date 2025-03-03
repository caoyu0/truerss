package truerss.util

import java.util.Date

import com.github.truerss.base.Entry

case class PreEntry(
   url: Option[String],
   title: Option[String],
   author: Option[String],
   publishedDate: Date,
   description: Option[String]
) {
  def toEntry: Entry = Entry(
    url = url.get,
    title = title.getOrElse("no-title"),
    author = author.getOrElse(""),
    description = description,
    publishedDate = publishedDate,
    content = None
  )
}
