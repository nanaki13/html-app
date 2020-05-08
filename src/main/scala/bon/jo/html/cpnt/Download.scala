package bon.jo.html.cpnt

import bon.jo.html.Types.FinalComponent
import org.scalajs.dom.html.Link
import org.scalajs.dom.raw.HTMLElement

import scala.xml.Node


case class Download(fileName: String, s: String) extends FinalComponent[Link] {
  def sBase64(string: String): String = new String(java.util.Base64.getEncoder.encode(string.getBytes))
  def getLink(string: String) = s"data:application/json;charset=utf-8;base64,${sBase64(string)}"
  override def xml(): Node = <a id={fileName} download={fileName} href={getLink(s)}>text file</a>

  def  updateLink(s : String) = {
      me.href = getLink(s)
  }

  override def init(parent: HTMLElement): Unit = {

  }

  override def id: String =fileName
}