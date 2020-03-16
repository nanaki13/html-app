package bon.jo.html

import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement


object Types {
  trait FinalComponent[Div <: HTMLElement] extends  XmlHtmlView[Div] with InDom[Div] with LeaveView[Div]
  trait ParentComponent[Div <: HTMLElement] extends  XmlHtmlView[Div] with InDom[Div] with NodeView[Div]

  type ClickableType = FinalComponent[Div] with OnClick[Div]
  trait Clickable extends FinalComponent[Div] with OnClick[Div]
}
