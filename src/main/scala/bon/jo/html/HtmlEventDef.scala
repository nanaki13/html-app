package bon.jo.html

import org.scalajs.dom.DragEvent
import org.scalajs.dom.html.Button
import org.scalajs.dom.raw.{Event, FocusEvent, HTMLElement, KeyboardEvent, MouseEvent}

import scala.concurrent.Future
import scala.scalajs.js


object HtmlEventDef:

  trait CaseEvent[A <: Event]:
    def apply(html: HTMLElement, a: A => Unit): js.Function1[A, _] =
      val f: js.Function1[A, _] = a
      html.addEventListener(toString, f)
      f

  case object blur extends CaseEvent[FocusEvent]

  case object click extends CaseEvent[MouseEvent]

  case object keyup extends CaseEvent[KeyboardEvent]

  case object change extends CaseEvent[Event]

  case object drag extends CaseEvent[DragEvent]

  case object dragstart extends CaseEvent[DragEvent]

  case object dragend extends CaseEvent[DragEvent]

  implicit class ExH(val html: HTMLElement) extends HtmlEventDef


trait HtmlEventDef:
  val html: HTMLElement

  def $blur: (FocusEvent => Unit) => js.Function1[FocusEvent, _] = HtmlEventDef.blur(html, _)

  def $click: (MouseEvent => Unit) => js.Function1[MouseEvent, _] = HtmlEventDef.click(html, _)

  def $keyup: (KeyboardEvent => Unit) => js.Function1[KeyboardEvent, _] = HtmlEventDef.keyup(html, _)

  def $change: (Event => Unit) => js.Function1[Event, _] = HtmlEventDef.change(html, _)

  def $drag: (DragEvent => Unit) => js.Function1[DragEvent, _] = HtmlEventDef.drag(html, _)

  def $dragstart: (DragEvent => Unit) => js.Function1[DragEvent, _] = HtmlEventDef.dragstart(html, _)

  def $dragend: (DragEvent => Unit) => js.Function1[DragEvent, _] = HtmlEventDef.dragend(html, _)

  def $Action(doThis: => Unit): Unit =
    $keyup { e => if e.keyCode == 13 then doThis }

  def $userCanDrag(): js.Function1[DragEvent, _] =
    $dragstart {
      e =>

        val deltaX = html.offsetLeft - e.clientX
        val deltaY = html.offsetTop - e.clientY


        lazy val ev: js.Function1[DragEvent, _] = $dragend {
          eEnd =>

            html.style.left = (eEnd.clientX + deltaX).toString + "px"
            html.style.top = (eEnd.clientY + deltaY).toString + "px"

            html.removeEventListener("dragend", ev)
        }
        ev
    }



