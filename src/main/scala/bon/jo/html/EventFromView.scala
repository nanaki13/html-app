package bon.jo.html

import org.scalajs.dom.Event

import scala.collection.mutable
import scala.scalajs.js
import bon.jo.html.IdView

trait EventFromView{
  self : IdView =>
  def eventsHadlers: mutable.Map[String, js.Function1[Event, _]] = EventFromView.eventsHadlers
  def myEvent[R <: Event](value: String): Option[js.Function1[R, _]] = EventFromView.eventsHadlers.get(value)
  def onClick(function: js.Function1[Event, _]): Unit = {
    eventsHadlers(id) = function
  }
}

object EventFromView{
  val eventsHadlers : mutable.Map[String,js.Function1[Event, _] ] = mutable.Map()
}