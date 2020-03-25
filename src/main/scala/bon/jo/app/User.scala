package bon.jo.app

import scala.scalajs.js

@js.native
trait User extends js.Object {
  val name: String
}

object User {
  def apply(name: String): User = js.Dynamic.literal(
    name = name
  ).asInstanceOf[User]
}