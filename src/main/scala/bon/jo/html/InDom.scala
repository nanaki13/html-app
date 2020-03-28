package bon.jo.html

import org.scalajs.dom.raw.HTMLElement
import bon.jo.html.DomShell.ExtendedElement

trait InDom[Me <: HTMLElement] {
  _: IdView =>
  lazy val me = DomShell.$[Me](id)

  def updateView() {}

  def init(parent: HTMLElement)

  def isInDom: Boolean = DomShell.$[Me](id) match {
    case null => false
    case a: Any if !scalajs.js.isUndefined(a) => true
    case _ => false
  }

  def removeFromView(): Unit = {
    this match {
      case view: EventFromView[Me] => {
        view.eventsHadlers.get(view.id) match {
          case Some(value) => me.removeEventListener("click", value); view.eventsHadlers.remove(view.id)
          case _ =>
        }
      }
      case _ =>
    }
    me.removeFromDom()

  }

}
