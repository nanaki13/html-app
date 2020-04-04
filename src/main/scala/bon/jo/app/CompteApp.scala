package bon.jo.app

import bon.jo.Logger
import bon.jo.app.{AppLoader, HtmlApp, HtmlAppFactory, User}
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.html.DomShell
import bon.jo.html.DomShell.{$, $c, ExtendedHTMLCollection, ExtendedElement}
import org.scalajs.dom.html.{Div, Input, Table, TableCell, TableRow}
import org.scalajs.dom.{document, raw}
import org.scalajs.dom.raw.{Element, HTMLCollection, HTMLElement, KeyboardEvent, MouseEvent}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSGlobalScope}
import scala.xml.{Elem, Group, MetaData, Node, Null, UnprefixedAttribute}


class CompteApp(app: Div, template: Template) extends HtmlApp[CompteTemplate](app, template) {

}

class CompteTemplate(override val user: User) extends Template with XmlTemplate {


  def EntrerEvent(el: HTMLElement) = {
    el.addEventListener("keyup",{ e : KeyboardEvent => {
      if(e.key =="Enter"){

      }
    }})
  }
  def UserCanUpdate(id: String) {
    UserCanUpdate($[HTMLElement](id))
  }

  def UserCanUpdate(el: HTMLElement) {
    el.addEventListener("click", (e: MouseEvent) => {
      el.innerText=""
      el.clear()
      el.addChild(<input></input>)
      el.firstChild.asInstanceOf[Input].addEventListener("click", (e: MouseEvent) => {
        val nVlaue = e.target.asInstanceOf[Input].value
        el.clear()
        el.appendChild(document.createTextNode(nVlaue))
      })
    })
  }

  var nbRow = 0
  val participe: Seq[String] = List("Jonathan", "Julia")
  val ratio: Seq[Float] = List(1 - 0.33F, 0.33F)

  val participAndRatio = participe zip ratio

  override def xml: Node = <div class="container">
    <div class="container-fluid">
      <table class="table" id="table">
        <tr>
          {participe.map(p => {
          <th id={p}>
            {p}
          </th>
        })}
        </tr>{createRowStart()}
      </table>
      <table class="table" id="total">
        <tr>
          <th></th>{participe.map(p => {
          <th>
            {p}
          </th>
        })}<th>
          Somme
        </th>
        </tr>
        <tr>
          <th>Total</th>{participe.map(p => {
          <td class="sub-tot" id={"tot-" + p}>
            0
          </td>
        })}<th id={"tot"}>
          0
        </th>
        </tr>
        <tr>
          <th>
            pourcentage en charge
          </th>{ratio.map(p => {
          <td class="ratio">
            {(p * 100).format}
            %
          </td>
        })}<th></th>
        </tr>
        <tr>
          <th>
            aurait du payer
          </th>{participe.map(p => {
          <td class="ba-tot" id={"ba-tot-" + p}>
            0
          </td>
        })}<th></th>
        </tr>
        <tr>
          <th>
            Au final
          </th>{participe.map(p => {
          <td class="ba alert" id={"ba-" + p}>
            0
          </td>
        })}<th></th>
        </tr>
      </table>
    </div>
  </div>

  lazy val table: Table = $[Table]("table")

  lazy val subTot: HTMLCollection = document.getElementsByClassName("sub-tot")
  lazy val balanceTot: HTMLCollection = document.getElementsByClassName("ba-tot")
  lazy val balance: HTMLCollection = document.getElementsByClassName("ba")
  lazy val ratioEls: HTMLCollection = document.getElementsByClassName("ratio")

  def createRowStart(): Elem = {
    nbRow += 1
    val el = <tr id={"r-" + nbRow}>
      {participe.map(p => {
        <td id={"r-" + p + "-" + nbRow}>
          {DomShell.inputXml("i-r-" + p + "-" + nbRow, "", inputClasses = p, dataSet = Map("owner" -> p))}
        </td>
      })}
    </tr>

    el

  }

  def createRow(): Unit = {
    nbRow += 1
    val current = nbRow
    val row = table.insertRow(-1).asInstanceOf[TableRow]

    row.setAttribute("id", "r-" + nbRow)
    participe.foreach(p => {
      val cell = row.insertCell(-1)
      cell.setAttribute(id, "r-" + p + "-" + nbRow)

      cell.appendChild(DomShell.inputHtml("i-r-" + p + "-" + nbRow, "", inputClasses = p, dataSet = Map("owner" -> p)))

    })

    row.addEventListener("keyup", { k: KeyboardEvent => {
      processEvent(k, current)
    }
    })
  }

  implicit class FloatFromat(float: Float) {
    def format: String = f"$float%.2f"
  }

  lazy val tot = $[TableCell]("tot")

  def computeTotal(newTot: Float) = {
    participAndRatio.foreach(t => {
      val (e, r) = t
      val somme = $[TableCell]("tot-" + e).innerHTML.toFloat
      $[TableCell]("tot-" + e).innerHTML = somme.format

      val balance = newTot * r
      $[TableCell]("ba-tot-" + e).innerHTML = balance.format

      val ba = somme - balance
      val baCell = $[TableCell]("ba-" + e)

      baCell.innerHTML = (ba).format
      if (ba >= 0) {
        baCell.classList.add("alert-success")
        baCell.classList.remove("alert-danger")
      } else {
        baCell.classList.remove("alert-success")
        baCell.classList.add("alert-danger")
      }


    })

  }

  def processEvent(k: KeyboardEvent, current: Int): Unit = if (k.key == "Enter") {
    if (table.rows.length < current + 2) {
      createRow()
    }
    val owner = k.target.asInstanceOf[Input].dataset("owner")
    $[Input]("i-r-" + owner + "-" + (current + 1)).focus()
  } else {
    val target = k.target.asInstanceOf[Input]
    val prev = target.value
    target.value = target.value.replaceAll(s"[^0-9,.]", "")
    if (prev == target.value) {
      participAndRatio.foreach(t => {
        val (e, r) = t
        if (target.classList.contains(e)) {
          val newSomme = $._class[Input](e).filter(_.value.nonEmpty).map(_.value.toFloat).sum
          $[TableCell]("tot-" + e).innerHTML = newSomme.toString
          val newTot = subTot.map(_.asInstanceOf[TableCell].innerHTML.toFloat).sum
          tot.innerHTML = newTot.toString

          computeTotal(newTot)
        }
      })

    }
  }

  override def init(parent: HTMLElement): Unit = {


    $[TableRow]("r-1").addEventListener("keyup", { k: KeyboardEvent => {
      processEvent(k, 1)
    }
    })
    ratioEls.map(_.asInstanceOf[HTMLElement]).foreach(UserCanUpdate)

    // participe.map($(_))
  }
}

object AppFact extends App with AppLoader {

  js.eval("var conf = {prod : false}")


  override val conf: Map[String, HtmlAppFactory[_]] = Map("app-compte" -> {
    new HtmlAppFactory[CompteTemplate](htmlAppFactory = new CompteApp(_, _), new CompteTemplate(_))
  })
  override val apps: List[String] = List("app-compte")


  loads(apps)
}