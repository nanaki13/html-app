package bon.jo.html

import bon.jo.html.HTMLDef.{$attr, $ref, $t, HtmlOps}
import org.scalajs.dom.html.Span

object CommonHtml {
  def spinner = $ref div { r =>
    r._class = "spinner-border"
    r.$attr("role" -> "status")
  }
  val closeClass = "closeClass"
  def closeBtn: Span = {
    (($attr span List("type" -> "button", "class" -> s"badge badge-secondary $closeClass", "aria-label" -> "Close")  ) +=
      $t("×")
      ).$to
  }
}
