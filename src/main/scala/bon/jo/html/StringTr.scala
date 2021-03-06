package bon.jo.html

import scala.scalajs.js
import scala.scalajs.js.JSON
import bon.jo.util.JsonTr

object StringTr:
  trait Out[A] extends (String => A)

  trait In[A] extends (A => String)

  trait OutIn[A]:
    def apply: Out[A]

    def unapply: In[A]

  object jsontr extends OutIn[js.Any]:
    override def apply: Out[js.Any] = JSON.parse(_)

    override def unapply: In[js.Any] = (e:js.Any) => JsonTr.string(e)
