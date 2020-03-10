package bon.jo.html

import bon.jo.html.DomShell.{$o, button}
import org.scalajs.dom.Event
import org.scalajs.dom.html.{Button, Element}

import scala.scalajs.js

trait OnClick[R <: Element] extends EventFromView with LeaveView[R] {


  override def updateView(): Unit = {
    myEvent(id).foreach((func: js.Function1[Event, _]) => {
      $o[R](id).foreach(e => {
        e.addEventListener("click",func)
      })
    })


  }
}

object OnClick {
  def apply(idp: String, label: String = ""): OnClick[Button] = new OnClick[Button]() {
    override def id: String = idp

    override def html(): Button = toElement(button(id = id, label))
  }
}