package bon.jo.html

import bon.jo.html.DomShell.{$o, button}
import org.scalajs.dom.html.{Button, Element}
import org.scalajs.dom.raw.HTMLElement

trait OnClick[R <: Element] extends EventFromView with LeaveView[R] {


  override def init(p: HTMLElement): Unit = {

    myEvent(id) match {
      case None => throw new Exception("dont find " + id)
      case Some(func) => $o[R](id) match {
        case None => throw new Exception("dont find " + id)
        case Some(e) => e.addEventListener("click", func)
      }
    }

  }
}

object ButtonHtml {
  def apply(idp: String, label: String = ""): OnClick[Button] = new OnClick[Button]() {
    override def id: String = idp

    override def html(): Button = toElement(button(id = id, label))


  }
}