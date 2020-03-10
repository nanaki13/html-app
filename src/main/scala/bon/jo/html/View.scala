package bon.jo.html

import bon.jo.html.DomShell.$
import org.scalajs.dom.Element
import org.scalajs.dom.raw.{HTMLCollection, HTMLElement}

import scala.collection.mutable
import scala.xml.{Node, NodeBuffer}

trait IdView{
  def id : String
}
trait _View[Root <: HTMLElement] extends IdView{

  def removeFromView(): Unit = DomShell.remove < id
  def toElement[E <: Element](n: Node): E =  BridgeXmlHtml.toElement(n)

  def toElementBase(n: NodeBuffer): HTMLCollection =  BridgeXmlHtml.toElementBase(n)
  def html(): Root
}
trait NodeView[Root <: HTMLElement] extends InDom with _View[Root]{

  val inDoms = mutable.ListBuffer[InDom]()



  def addTo(id: String): Unit = {
    $[HTMLElement](id).appendChild(html())
    updateView()
  }

  def addTo(el: HTMLElement): Unit = {
    el.appendChild(html())
    updateView()
  }
  def add[T <: InDom](inDom : T):T ={
    inDoms += inDom
    inDom
  }

  final override def updateView(): Unit = inDoms.foreach(_.updateView())
}
abstract case class View[Root <: HTMLElement](html : Root) extends LeaveView[Root] {

  override def updateView(): Unit = {}
}
abstract case class XmlView[Root <: HTMLElement](xml : Node) extends LeaveView[Root] {

  override def updateView(): Unit = {}

  override def html(): Root = BridgeXmlHtml.toElement(xml)
}
trait LeaveView[Root <: HTMLElement] extends InDom with _View[Root] with IdView {

  def html(): Root
  def addTo(id: String): Unit = {
    $[HTMLElement](id).appendChild(html())
    updateView()
  }
  def addTo(el: HTMLElement): Unit = {
    el.appendChild(html())
    updateView()
  }

}

trait BridgedView[Root <: HTMLElement,_1] extends _View[Root]{
  def conversion(_1 : _1) : Root
  def myFormat() : _1
  def html(): Root = conversion(myFormat())

}
trait XmlHtmlView[Root <: HTMLElement] extends BridgedView[Root,Node]{
  def conversion(n: Node): Root =  BridgeXmlHtml.toElement(n)
  def xml() : Node
  final override def myFormat(): Node = xml()
}
