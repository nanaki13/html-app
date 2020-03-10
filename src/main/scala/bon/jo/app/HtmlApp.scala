package bon.jo.app

import bon.jo.game.html.Template
import org.scalajs.dom.html.Div

abstract class HtmlApp[Tp <: Template](app: Div, template: Template) {
      val typedTemplate: Tp = template.asInstanceOf[Tp]
}
