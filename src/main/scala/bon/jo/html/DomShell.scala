package bon.jo.html

import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{Element, HTMLCollection, HTMLElement}

import scala.scalajs.js
import scala.xml.{Elem, Group, Node}

object DomShell {
  def log(m: Any): Unit = org.scalajs.dom.window.console.log(m)

  def deb(): Unit = js.special.debugger()


  implicit class ExtendedElement(val e: HTMLElement) {
    def css(s: String): Unit = {
      e.classList.add(s)
    }

    def clear(): Unit = e.children.foreach(e => e.parentNode.removeChild(e))

    def addChild(node: Node): Unit ={
      e.appendChild($c(node))
    }
    def clearAndReplace(el: List[HTMLElement]): Boolean = DomShell.clearAndAdd(e, el)

    def safeRemoveChild(el: HTMLElement): Any = if (e.contains(el)) {
      e.removeChild(el)
    }

  }

  implicit class ExtendedHTMLCollection(val e: HTMLCollection) extends Iterable[Element] {
    def iterator: Iterator[Element] = {
      (0 until e.length).map(e(_)).iterator
    }
  }



  def form(seq: Node*): Elem = <form>
    {Group(seq)}
  </form>

  def form(in: Node): Elem = <form>
    {in}
  </form>

  def inputXml(name: String, label: String, value: Any = "", _type: String = "text"): Elem = <div class="form-group">
    <label for={s"" + name} class="form-label">
      {label}
    </label> <input class="form-control" name={s"" + name} id={s"" + name} placeholder={"" + label} value={"" + value} type={_type}/>
  </div>

  def inputHtml(name: String, label: String, value: Any = ""): Div = BridgeXmlHtml.toElement(inputXml(name, label, value))

  def button(id: String, text: String, class_ : String = ""): Elem = <button id={"" + id} class={"btn btn-primary" + class_} type="button">
    {text}
  </button>

  object $c {
    def withInner[D <: Div](html: String): D = {
      val ret = $c[D]
      ret.innerHTML = html
      ret
    }

    def apply(node: Node): Element = {
      $c.withInner[Div](node.mkString).children(0)
    }

    def apply[D <: Div](): D = {
      document.createElement("div").asInstanceOf[D]
    }

    def apply[R <: Element](html: String): R = {
      $c.withInner[Div](html).children(0).asInstanceOf[R]
    }
  }

  def $o[R <: Element](id: String): Option[R] = {
    Option($[R](id))
  }

  object $ {

    def apply[R <: Element](id: String): R = {
      val ret = document.getElementById(id)
      if (ret != null) {
        ret.asInstanceOf[R]
      } else {
        null.asInstanceOf[R]
      }
    }

    def _class[R <: Element](cl: String): Iterable[R] = document.getElementsByClassName(cl).map(_.asInstanceOf[R])
  }

  lazy val _remove: DomShellEllemnt[Boolean] = {
    id: String =>
      val e: HTMLElement = $[HTMLElement](id)
      if (e != null) {
        e.parentNode.removeChild(e)
        true
      } else {
        false
      }
  }

  lazy val _removeAll: DomShellEllemnt[Boolean] = {
    class_ : String =>
      $._class[Element](class_).map(e => {
        e.parentNode.removeChild(e)
      }).nonEmpty
  }


  /**
   *
   * @return
   */
  def remove: DomShellEllemnt[Boolean] = {
    id: String =>

      val e: HTMLElement = $[HTMLElement](id)
      if (e != null) {
        e.parentNode.removeChild(e)
        true
      } else {
        false
      }
  }

  def removeAll: DomShellEllemnt[Boolean] = _removeAll

  def addAll: DomShellFunction[(Element, List[Element]), Boolean] = l => {
    l._2.map(l._1.appendChild).nonEmpty
  }

  def clearAndAdd(a: HTMLElement, b: List[HTMLElement]): Boolean = {
    a.clear()
    addAll(a, b)
  }

  def allOf[R <: Element]: DomShellEllemnt[Iterable[R]] = $._class[R]

  trait DomShellEllemnt[R] extends (String => R) {
    def <(id: String): R = this apply id
  }

  trait DomShellFunction[P, R] extends (P => R) {
    def <(p: P): R = this apply p
  }

}
