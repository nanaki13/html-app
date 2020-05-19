package bon.jo.test

import bon.jo.app.{HtmlApp, User}
import bon.jo.game.html.Template
import bon.jo.game.html.Template.XmlTemplate
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement

import scala.xml.Node

class Test(app: Div, template: Template) extends HtmlApp[Test](template: Template) with Template with XmlTemplate {


  override val user: User = {
    (this : HtmlApp[_]).user
  }


  def this() = this(null, null)


  override def xml: Node = <div id="test"></div>



  override def init(p: HTMLElement): Unit = {
    
  }
}

