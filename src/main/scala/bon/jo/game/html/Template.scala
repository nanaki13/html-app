package bon.jo.game.html

import bon.jo.html.{IdView, InDom}
import org.scalajs.dom.html.Div

import scala.xml.Node

trait Template extends InDom[Div] with IdView{
  val id: String = "root"

  def body: String
}
object Template{
  trait XmlTemplate{
     _ :  Template =>
    def xml : Node

    final override def body: String = {
      xml.mkString
    }
  }
}
