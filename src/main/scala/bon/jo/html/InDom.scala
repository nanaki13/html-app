package bon.jo.html

import org.scalajs.dom.raw.HTMLElement

trait InDom[Me <: HTMLElement]{
  _ : IdView =>
  lazy val me = DomShell.$[Me](id)

  def updateView(){}
  def init(parent : HTMLElement)
}
