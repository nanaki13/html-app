package bon.jo.app.service

import bon.jo.app.RequestHttp.{DELETE, GET, PATCH, POST}
import bon.jo.app.{Response, User}

import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits._

case class DistantService[A](url: String)
                            (implicit read: js.Any => A, write: A => String, user: User) {


  def save(m: A): Future[Response] = {
    POST.send(dest = url, body = m)
  }

  def update(m: A): Future[Response] = {
    PATCH.send(dest = url, body = m)
  }

  def get(id: Int): Future[Option[A]] = {
    GET.send(dest = url + "/" + id).map {
      e => e.bodyAsJson.map(read)
    }
  }

  def getAll: Future[Option[List[A]]] = {
    GET.send(dest = url).map {
      e =>
        e.bodyAsJson.map(ee => {
          ee.asInstanceOf[js.Array[js.Any]].map(read).toList
        })
    }
  }

  def delete(id: Int): Future[Response] = {
    DELETE.send(dest = url + "/" + id)
  }

}
