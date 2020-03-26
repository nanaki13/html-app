package bon.jo.app

import java.util.Base64

import bon.jo.app.RequestHttp.{GET, Method}
import bon.jo.html.DomShell
import org.scalajs.dom.experimental.URLSearchParams

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.scalajs.js
import scala.scalajs.js.JSON

object Auth {


  class AuthFromSearch() extends Auth {
    override def extractToken: Option[String] = {
      val paramParser: URLSearchParams = new URLSearchParams(org.scalajs.dom.window.location.search)
      Option(paramParser.get("token")).map(e => {
        org.scalajs.dom.window.localStorage.setItem("token", e)
        e
      })
    }

    override protected def selfEndOk(): Unit = {
      println("loged from url");
      org.scalajs.dom.window.history.pushState(null, org.scalajs.dom.document.title, org.scalajs.dom.window.location.pathname)
    }

    override protected def selfEndKo(): Unit = {
      org.scalajs.dom.window.history.pushState(null, org.scalajs.dom.document.title, org.scalajs.dom.window.location.pathname)
    }
  }

  class AuthFromStore extends Auth {
    override def extractToken: Option[String] = Option(org.scalajs.dom.window.localStorage.getItem("token"))

    override protected def selfEndOk(): Unit = {
      println("loged from Store")
    }

    override protected def selfEndKo(): Unit = org.scalajs.dom.window.localStorage.removeItem("token")
  }

  def parse(resp: String): js.Any = {
    JSON.parse(new String(Base64.getUrlDecoder.decode(resp.substring(resp.indexOf('.') + 1, resp.lastIndexOf('.')))))
  }

  def apply(paramSearch: String): Auth = {
    implicit val s = paramSearch
    new AuthFromSearch
  }

  def apply(): Auth = {
    new AuthFromStore
  }


  def doAuth()(ok: User => Unit, ko: => Unit): Future[User] = {


    case class Acc(var token: Option[String] = None, var auth: Option[Auth] = None)
    val method = List(new AuthFromSearch, new AuthFromStore)
    val toeknParsingResult = method.foldLeft(Acc())((acc, auth) => {
      acc match {
        case Acc(Some(_), _) => acc
        case _ => auth.extractToken match {
          case Some(value) => acc.copy(Some(value), Some(auth))
          case None => acc
        }
      }

    })
    toeknParsingResult match {
      case Acc(Some(token), Some(auth)) => auth.validate(token)
      case _ => Future.successful(User("Anonym"))
    }

  }
}

sealed abstract class Auth {

  implicit val ex: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  def extractToken: Option[String]

  protected def selfEndOk(): Unit

  protected def selfEndKo(): Unit

  def validate(token: String): Future[User] = {
    GET.withOkStatus(204).send(dest = s"/auth/verify", headers = List(("Authorization", "Bearer " + token)))
      .map(_.body[User]) map {
      _.getOrElse(User("Anonym"))
    }

  }
}