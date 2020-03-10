package bon.jo.game.html

import bon.jo.html.InDom

import scala.xml.Node

trait Template extends InDom {
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
