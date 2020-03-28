package bon.jo.html

import bon.jo.html.DomShell.{$o, button}
import org.scalajs.dom.html.{Button, Element, Link}
import org.scalajs.dom.raw.HTMLElement

trait OnClick[R <: Element] extends EventFromView[R] with InDom[R] with IdView {

  override def init(p: HTMLElement): Unit = {

    myEvent(id) match {
      case None => throw new Exception("dont find " + id)
      case Some(func) => me.addEventListener("click", func)
    }
  }


}
object OnClick{
  type BaseClick[ClickType<: Element] =  OnClick[ClickType] with InDom[ClickType] with IdView
  def apply[ClickType <: Element](idp : String): BaseClick[ClickType] = new OnClick[ClickType]()  with  InDom[ClickType] with IdView{
    override def id: String = idp
    override def updateView(): Unit = {}
  }
}
trait ButtonHtml extends LeaveView[Button] with InDom[Button] with OnClick[Button]
object ButtonHtml {
  type ButtonType = LeaveView[Button] with InDom[Button] with OnClick[Button]

  def apply(idp: String, label: String = ""): ButtonType = new LeaveView[Button] with InDom[Button] with OnClick[Button] {
    override def id: String = idp

    override def html(): Button = toElement(button(id = id, label))

    override def updateView(): Unit = {}
  }
}