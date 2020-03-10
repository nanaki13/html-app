package bon.jo.test

import bon.jo.app.HtmlApp
import bon.jo.game.Const
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import bon.jo.html.DomShell
import org.scalajs.dom.html.Div

import scala.xml.Node

class Test(app: Div, template: Template) extends HtmlApp[Test](app: Div, template: Template) with Template with XmlTemplate {

  DomShell.log(Const.urlCardGame)



  def this() = this(null, null)





  override def xml: Node = <div id="test"></div>

  override def updateView(): Unit = {

  }

}

