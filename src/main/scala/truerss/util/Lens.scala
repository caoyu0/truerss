package truerss.util

import shapeless._
import truerss.models.Source

object Lens {
  val normalized = lens[Source].normalized
  val count = lens[Source].count
  val id = lens[Source].id
  val state = lens[Source].state
}
