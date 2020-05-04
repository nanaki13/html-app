package bon.jo.html

import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import bon.jo.html.DomShell.{ExtendedElement}
import bon.jo.phy.Obs

import scala.xml.Node

trait InDom[Me <: HTMLElement] {
  _: IdView =>
  lazy val me = DomShell.$[Me](id)


  def init(parent: HTMLElement)

  def isInDom: Boolean = DomShell.$[Me](id) match {
    case null => false
    case a: Any if !scalajs.js.isUndefined(a) => true
    case _ => false
  }

  def removeFromView(): Unit = {
    me.removeFromDom()
  }



}
object InDom{

  class simple[Me<: HTMLElement](node : Node) extends  InDom[Me]() with IdView with XmlHtmlView[Me] {
    override def init(parent: HTMLElement): Unit = {
      parent.appendChild(html())
    }

    override def id: String = node \@ "id"

    override def xml(): Node = node
  }
  class clk[Me<: HTMLElement](node : Node) extends  simple[Me](node) with Clickable[Me]
  def apply[Me<: HTMLElement](node : Node): InDom[Me] with XmlHtmlView[Me]= new simple(node)
  def clk[Me<: HTMLElement](node : Node): InDom[Me]with XmlHtmlView[Me] = new clk(node)
}
trait Clickable[Me <: HTMLElement] extends InDom[Me] with IdView {
  val obs =  Obs.once[MouseEvent]()

  override def init(parent: HTMLElement): Unit = {
    me.addEventListener("click",obs.newValue)
  }
  def obsClick(): Obs[MouseEvent] = obs
}
