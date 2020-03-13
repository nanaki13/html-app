package bon.jo.test

import bon.jo.app.HtmlApp
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import org.scalajs.dom.html.Div

import scala.xml.Node

class Test(app: Div, template: Template) extends HtmlApp[Test](app: Div, template: Template) with Template with XmlTemplate {





  def this() = this(null, null)





  override def xml: Node = <div id="test"></div>

  override def updateView(): Unit = {

  }

}

