package bon.jo.test

import java.util.UUID

import bon.jo.html.DomShell.$
import org.scalajs.dom.raw

import scala.xml.Elem

trait DomCpnt[H <: raw.Element] {

  val id: String = UUID.randomUUID().toString

  def html: H = $[H](id)

  def xml: Elem
}

object DomCpnt {
  def apply[E <: raw.Element](iderXml: String => Elem): DomCpnt[E] = new DomCpnt[E] {
    override def xml: Elem = iderXml(id)
  }
}