package bon.jo.html

import org.scalajs.dom.raw.{Event, FocusEvent, HTMLElement, KeyboardEvent, MouseEvent}


object HtmlEventDef {

  trait CaseEvent[A <: Event] {
    def apply(html: HTMLElement, a: A => Unit): Unit = {
      html.addEventListener[A](toString, a)

    }
  }

  case object blur extends CaseEvent[FocusEvent]

  case object click extends CaseEvent[MouseEvent]
  case object keyup extends CaseEvent[KeyboardEvent]
  case object change extends CaseEvent[Event]

  implicit class ExH(val html: HTMLElement) extends HtmlEventDef

}

trait HtmlEventDef {
  val html: HTMLElement

  def $blur: (FocusEvent => Unit) => Unit = HtmlEventDef.blur(html, _)
  def $click: (MouseEvent => Unit) => Unit = HtmlEventDef.click(html, _)
  def $keyup: (KeyboardEvent => Unit) => Unit = HtmlEventDef.keyup(html, _)
  def $change: (Event => Unit) => Unit = HtmlEventDef.change(html, _)
  def $Action(doThis: => Unit): Unit = {
    $keyup{e => if(e.keyCode == 13) doThis}
  }

}

