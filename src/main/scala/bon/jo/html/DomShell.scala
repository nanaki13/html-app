package bon.jo.html

import bon.jo.Logger
import org.scalajs.dom.html.{Div, Input}
import org.scalajs.dom.raw._
import org.scalajs.dom.{document, raw}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
//import scala.scalajs.js.Promise
import scala.xml.{Elem, MetaData, Node, Null, UnprefixedAttribute}
import scala.xml.Group

object DomShell {

  case class StackObs[A](var clients: List[A => Unit] = Nil) extends Obs[A] {
    override def suscribe(client: A => Unit): Unit = clients = client :: clients

    override def newValue(a: A): Unit = if(clients.nonEmpty) clients.foreach(_ (a)) else  Logger.log("no client for observed value  : "+a)

    override def clearClients: Unit = clients = Nil
  }

  trait Obs[A] {
    def suscribe(client: A => Unit): Unit

    def newValue(a: A): Unit

    def clearClients: Unit
  }


  class OnceObs[A](private var client: A => Unit = null) extends Obs[A] {
    def clearClients: Unit = {
      client = null
    }

    override def suscribe(clientp: A => Unit): Unit = client = clientp

    override def newValue(a: A): Unit = {
      if(client != null) client(a) else Logger.log("no client for observed value  : "+a)
    }

    def toMany = new StackObs[A](if (client != null) client :: Nil else Nil)
  }

  object Obs {
    val alls: mutable.Map[String, Obs[_]] = mutable.Map[String, Obs[_]]()

    def once[A](): OnceObs[A] = {
      val ret = new OnceObs[A]() {}
      ret
    }

    def once[A](client: A => Unit): OnceObs[A] = {
      val ret = new OnceObs[A](client) {}
      ret
    }

    def get[A](id: String): Obs[A] = {

      val ret = alls.get(id)
      if (ret.isEmpty) {
        val n = new StackObs[A]()
        alls(id) = n
        n
      } else {
        ret.get.asInstanceOf[Obs[A]]
      }
    }


  }

  trait GiveItToMe extends (String => String)

  trait SendItToYou extends (String => String)

  trait Diopter {
    def giveItToMe: GiveItToMe

    def sendItToYou: SendItToYou
  }

  object PercentDioptre extends Diopter {
    override def giveItToMe: GiveItToMe = _.cleanNonNumber + " %"

    override def sendItToYou: SendItToYou = _.replace(" %", "")
  }

  def log(m: Any): Unit = org.scalajs.dom.window.console.log(m)



  implicit class ExtendedNode(val element: org.scalajs.dom.raw.Node) {

    def removeFromDom(): raw.Node = element.parentNode.removeChild(element)

    def addChild(node: Node): Unit = {
      element.appendChild($c(node))
    }

    def addChild(node: String): Unit = {
      val n = $c[HTMLElement](node)
      element.appendChild(n)
    }


    def safeRemoveChild(el: HTMLElement): Any = if (element.contains(el)) {
      element.removeChild(el)
    }
  }

  implicit class NumberFormat(val s: String) {
    def cleanNonNumber = s.replaceAll(s"[^0-9,.]", "")
  }

  implicit class ExtendedElement(override val element: org.scalajs.dom.raw.HTMLElement) extends ExtendedNode(element) {
    def css(s: String): Unit = {
      element.classList.add(s)
    }


    def clk(): Obs[MouseEvent] = {

      val obs = Obs.get[MouseEvent](element.id + "-clk-obs")
      element.addEventListener("click", (e: MouseEvent) => obs.newValue(e))
      obs
    }

    def clkOnce(): OnceObs[MouseEvent] = {

      val obs = Obs.once[MouseEvent]()

      element.addEventListener("click", (e: MouseEvent) => obs.newValue(e))
      obs
    }

    import scala.concurrent.ExecutionContext.Implicits._

    def EnterEvent(el: HTMLElement)(f: js.Function1[KeyboardEvent, _]): Unit = {
      val filterKey: js.Function1[KeyboardEvent, Boolean] = _.key == "Enter"
      val composedFunction: js.Function1[KeyboardEvent, _] = { e =>
        if (filterKey(e)) {
          f(e)
        }
      }
      el.addEventListener("keyup", composedFunction)
    }

    def CaputreResult(resConsumr: String => Unit,optionRead : Option[() => String]): Unit = {
      EnterEvent(element) {
        e =>
          optionRead match {
            case Some(value) =>  resConsumr(value())
            case None => resConsumr(e.target.asInstanceOf[Input].value)
          }

      }
    }

    def ValueUserEnter(optionRead : Option[() => String]): Future[String] = {
      val p = Promise[String]()

      element.CaputreResult(res => p.success(res),optionRead)
      p.future

    }

    def UserCanUpdate(diopter: Option[Diopter] = None, inputView: Option[String => HTMLElement] = None
      ,read : Option[HTMLElement => String] = None
                     )(implicit fact : () => Obs[String]): Obs[String] = {

      element.style.cursor = "pointer"

     val obs = fact()

      lazy val jsF: js.Function1[MouseEvent, _] = (e: MouseEvent) => {
        def sendItToYou: String => String = diopter.map(_.sendItToYou).getOrElse(identity[String])

        def giveItToMe: String => String = diopter.map(_.giveItToMe).getOrElse(identity[String])

        element.removeEventListener("click", jsF)
        val old = element.innerText

        val conservseWoth = element.clientWidth

        element.innerText = ""
        element.clear()
        var optionRead : Option[() => String] = None
        inputView match {
          case Some(value) => {
            element.appendChild(value(sendItToYou(old)))

            optionRead = Some{
              () =>  read.get(element.children.head.asInstanceOf[HTMLElement])
            }
          }
          case None => element.addChild(<input value={sendItToYou(old)}>
          </input>)
        }


        val in = element.firstChild.asInstanceOf[Input]
        element.style.width = conservseWoth.toString + "px"
        in.style.width = conservseWoth.toString + "px"
        //  in.style.width = conservseWoth.toString+"px"
        in.ValueUserEnter(optionRead) foreach {
          ()
          (e) =>
            val nVlaue = giveItToMe(e)
            element.clear()
            element.appendChild(document.createTextNode(nVlaue))
            element.addEventListener("click", jsF)
            obs.newValue(e)
        }
      }
      element.addEventListener("click", jsF)
      obs
    }

    def clear(): Unit = element.children.foreach(a => element.removeChild(a))

    def inDom: Boolean = {
      Logger.log(element.id + " = " + document.getElementById(element.id))

      document.getElementById(element.id) != null
    }

    def clearAndReplace(el: List[HTMLElement]): Boolean = DomShell.clearAndAdd(element, el)

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

  def simpleInputXml(name: String, label: String, value: Any = "", _type: String = "text"
                     , inputClasses: String = "", dataSet: Map[String, String] = Map.empty
                    ): Elem = {

    val metaDataAgg: MetaData = Null
    val metaData = dataSet.foldLeft(metaDataAgg)((md, kv) => md.copy(new UnprefixedAttribute("data-" + kv._1, kv._2, Null)))
    val class_ = "form-control" + (if (inputClasses.nonEmpty) {
      " " + inputClasses
    } else "")
    val in = <input class={class_} name={s"" + name} id={s"" + name} placeholder={"" + label} value={"" + value} type={_type}/> //
    in.copy(attributes = in.attributes.append(metaData))
  }

  def inputXml(name: String, label: String, value: Any = "", _type: String = "text"
               , inputClasses: String = "", dataSet: Map[String, String] = Map.empty
              ): Elem = {

    val metaDataAgg: MetaData = Null
    val metaData = dataSet.foldLeft(metaDataAgg)((md, kv) => md.copy(new UnprefixedAttribute("data-" + kv._1, kv._2, Null)))

    <div class="form-group">
      <label id={s"l-" + name} for={s"" + name} class="form-label">
        {label}
      </label>{val class_ = "form-control" + (if (inputClasses.nonEmpty) {
      " " + inputClasses
    } else "")
    val in = <input class={class_} name={s"" + name} id={s"" + name} placeholder={"" + label} value={"" + value} type={_type}/> //
    in.copy(attributes = in.attributes.append(metaData))}
    </div>
  }

  def inputHtml(name: String, label: String, value: Any = "", inputClasses: String = "", dataSet: Map[String, String] = Map.empty): Div = BridgeXmlHtml.toElement(inputXml(name, label, value, inputClasses = inputClasses, dataSet = dataSet))

  def simpleInputHtml(name: String, label: String, value: Any = "", inputClasses: String = "", dataSet: Map[String, String] = Map.empty): Div = BridgeXmlHtml.toElement(simpleInputXml(name, label, value, inputClasses = inputClasses, dataSet = dataSet))

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
