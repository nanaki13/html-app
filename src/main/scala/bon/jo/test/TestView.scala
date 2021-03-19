package bon.jo.test

import bon.jo.app.User
import bon.jo.html.DomShell.$c
import org.scalajs.dom.raw

import scala.xml.Elem

case class Memo(id: Integer,title : String, content: String, user: User)

object TestView  {

  import XmlRep._

  implicit val userXml: XmlRep[User] = XmlRep[User] {
    memo => <div>{memo.name}</div>
  }

  implicit val memoXml: XmlRep[Memo] = XmlRep[Memo] {
    memo => <div><h1>{memo.title}</h1>
      {memo.content}<div>user :
        {memo.user.xml}
      </div>
    </div>
  }




}

trait XmlRep[A] {
  def xml(memo: A): Elem
}

object XmlRep {
  implicit class ListRep[A : XmlRep ]( seq : Iterable[A] ) {
    def xml: Iterable[Elem] = seq.map(_.xml)
  }
  implicit class PrXml[B](b: B)(implicit tr: XmlRep[B]) {
    def xml: Elem = tr.xml(b)
    def html: raw.Element = $c(xml)
  }

  def apply[A](a : A => Elem) : XmlRep[A] = b => a(b)

}
