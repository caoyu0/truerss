package truerss.system

import akka.actor.{Actor, ActorLogging, Props}
import akka.io.IO

import scala.slick.jdbc.JdbcBackend.DatabaseDef
import spray.can.Http

import truerss.api.RoutingService
import truerss.db.DbActor
import truerss.models.CurrentDriver

/**
 * Created by mike on 2.8.15.
 */
class SystemActor(dbDef: DatabaseDef, driver: CurrentDriver) extends Actor
  with ActorLogging {

  implicit val system = context.system

  val dbRef = context.actorOf(Props(new DbActor(dbDef, driver)), "db")

  val networkRef = context.actorOf(Props(new NetworkActor), "network")

  val sourcesRef = context.actorOf(Props(new SourcesActor(self, networkRef)), "sources")

  val proxyRef = context.actorOf(Props(new ProxyServiceActor(dbRef, networkRef, sourcesRef)), "proxy")

  val api = context.actorOf(Props(new RoutingService(proxyRef)), "api")

  IO(Http) ! Http.Bind(api, interface = "localhost", port = 8000)


  def receive = {
    case x => proxyRef forward x
  }

}
