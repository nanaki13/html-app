package bon.jo.html

import bon.jo.html.DomShell.$
import org.scalajs.dom.Element
import org.scalajs.dom.raw.{HTMLCollection, HTMLElement}

import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import scala.xml.{Node, NodeBuffer}

trait IdView {
  def id: String
}

trait _View[Root <: HTMLElement] extends InDom[Root] with IdView {



  def toElement[E <: Element](n: Node): E = BridgeXmlHtml.toElement(n)

  def toElementBase(n: NodeBuffer): HTMLCollection = BridgeXmlHtml.toElementBase(n)

  def html(): Root

  def addTo(id: String): Unit = {
    val parent = $[HTMLElement](id)
    parent.appendChild(html())
    init(parent)
  }

  def addTo(el: HTMLElement): Unit = {
    el.appendChild(html())
    init(el)
  }

}
trait LeaveView[Root <: HTMLElement] extends InDom[Root] with _View[Root] with IdView {

  def html(): Root



}
trait NodeView[Root <: HTMLElement] extends InDom[Root] with _View[Root] {

  val inDoms = mutable.ListBuffer[InDom[_]]()


  def add[T <: InDom[_]](inDom: T): T = {
    inDoms += inDom
    inDom
  }

  override def init(parent: HTMLElement): Unit = {
    if (me == null) {
      throw new Exception("On a pas " + id + " dans le dom")
    }
    inDoms.foreach(_.init(me))

  }
}

abstract case class View[Root <: HTMLElement](html: Root) extends LeaveView[Root] {


}

abstract case class XmlView[Root <: HTMLElement](xml: Node) extends LeaveView[Root] {
  @JSExport
  override def html(): Root = BridgeXmlHtml.toElement(xml)
}



trait BridgedView[Root <: HTMLElement, _1] extends _View[Root] {
  def conversion(_1: _1): Root

  def myFormat(): _1

  def html(): Root = conversion(myFormat())

}

trait XmlHtmlView[Root <: HTMLElement] extends BridgedView[Root, Node] {
  def conversion(n: Node): Root = BridgeXmlHtml.toElement(n)

  def xml(): Node

  final override def myFormat(): Node = xml()
}
