package bon.jo.app

import bon.jo.html.DomShell
import org.scalajs.dom.{Event, XMLHttpRequest}

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.{JSON, Promise}

object RequestHttp {
  def apply[R](method: Method, body: R)(response: Option[R => Unit])(st: Int => Unit)(implicit urlDesr: String,
                                                                                      writeRequest: R => js.Any,
                                                                                      readResponse: js.Any => R): RequestHttp = {
    implicit val st_ : Int => Unit = st
    implicit val bodyrequest: js.Any = writeRequest(body)
    implicit val hearder: Seq[(String, String)] = Nil
    val f: Option[js.Any => Unit] = response.map(res => {
      readResponse.andThen(r => res(r))

    })
    method.apply(f)
  }

  def apply[R](method: Method, urlDesr: String, pathSuffix: String)(response: Option[R => Unit])(st: Int => Unit)
              (implicit readResponse: js.Any => R): RequestHttp = {
    implicit val bodyrequest: js.Any = null
    implicit val st_ : Int => Unit = st
    implicit val path: String = urlDesr + pathSuffix
    implicit val hearder: Seq[(String, String)] = Nil
    val f: Option[js.Any => Unit] = response.map(res => {
      readResponse.andThen(r => res(r))

    })
    method.apply(f)

  }

  sealed abstract class Method(val okStatus: Int) {
    val name: String = this.toString

    def okStatus(status: Int): Boolean = okStatus == status

    def apply(reponseConsumer: Option[js.Dynamic => Unit], okProcess: Option[() => Unit] = None, okRawProcess: Option[String => Unit] = None)(
      implicit an: js.Any,
      urlDesr: String,
      _headers: Seq[(String, String)],
      st: Int => Unit): RequestHttp = {
      this match {
        case RequestHttp.POST => new POST(reponseConsumer)
        case RequestHttp.GET => new GET(reponseConsumer = reponseConsumer, rawConsumer = okRawProcess)
        case RequestHttp.PATCH => new PATCH(reponseConsumer)
        case RequestHttp.DELETE => new DELETE(okProcess)
      }
    }
  }

  case object POST extends Method(201)

  case object GET extends Method(200) {
    def `doRaw`(url: String)(rwoConsume: String => Unit) {
      implicit val body: js.Any = null
      implicit val des: String = url
      implicit val s: Int => Unit = DomShell.log
      implicit val hearder: Seq[(String, String)] = Nil
      val get = this (None, Some(rwoConsume))
      get.prepare()
      get.send()
    }


    def `doRaw`(dest: String, headers: List[(String, String)])(implicit status: Int => Unit): Unit = {
      implicit val body: js.Any = null
      implicit val des: String = dest
      implicit val _headers: Seq[(String, String)] = headers
      val get = this (None, None)
      get.prepare()
      get.send()
    }


  }

  case object PATCH extends Method(204)

  case object DELETE extends Method(204)

  case class POST(reponseConsumer: Option[js.Dynamic => Unit])(implicit an: js.Any, urlDesr: String, status: Int => Unit) extends RequestHttp(urlDesr, POST, an, reponseConsumer, Some(status), None)

  case class GET(reponseConsumer: Option[js.Dynamic => Unit], rawConsumer: Option[String => Unit])
                (implicit an: js.Any, urlDesr: String, status: Int => Unit, _headers: Seq[(String, String)])
    extends RequestHttp(
      urlDesr,
      GET,
      an,
      reponseConsumer,
      Some(status),
      None,
      reponseRawConsumer = rawConsumer,
      headers = _headers) {
    override def send(): Unit = {
      request.send(null)
    }


  }

  case class PATCH(reponseConsumer: Option[js.Dynamic => Unit])(implicit an: js.Any, urlDesr: String, status: Int => Unit) extends RequestHttp(urlDesr, PATCH, an, reponseConsumer, Some(status))

  case class DELETE(okProcess: Option[() => Unit])(implicit an: js.Any, urlDesr: String, status: Int => Unit) extends RequestHttp(urlDesr, DELETE, an, None, Some(status), okProcess) {
    override def send(): Unit = {
      request.send(null)
    }
  }

}

import scala.concurrent.ExecutionContext.Implicits._

case class Response(var body: String = null, var status: Int = -1)

class NEWRequestHttp(urlDesr: String,
                     method: RequestHttp.Method, headers: Seq[(String, String)] = Nil) {
  val request = new XMLHttpRequest

  def open(): Unit = request.open(method.name, urlDesr)

  def okStatus(status: Int): Boolean = method.okStatus(status)

  def sendBody(an: js.Any): Future[Response] = {
    prepare()


    new Promise[Response]((resolve, reject) => {
      println("On EST FDAN LA PROMESSE")
      request.send(an)
      request.onreadystatechange = (e: Event) => {

        if (request.readyState == XMLHttpRequest.DONE) {
          val resp: Response = Response(request.response.toString,request.status)
          if (okStatus(request.status)) {
            resolve(resp)
          } else {
            reject(request.status)
          }
        }
      }
    }).toFuture

  }

  def contentString(): String = {
    request.response.toString
  }


  def prepare(): Unit = {

    open()
    val makeHeader: ((String, String)) => Unit = request.setRequestHeader _ tupled _
    //Envoie les informations du header adaptées avec la requête

    request.setRequestHeader("Content-Type", "application/json");
    headers.foreach(makeHeader)

  }
}

abstract sealed class RequestHttp(urlDesr: String,
                                  method: RequestHttp.Method,
                                  an: js.Any,
                                  reponseConsumer: Option[js.Dynamic => Unit],
                                  statusConsumer: Option[Int => Unit] = None,
                                  okProcess: Option[() => Unit] = None,
                                  reponseRawConsumer: Option[String => Unit] = None,
                                  param: Map[String, Any] = Map(),
                                  headers: Seq[(String, String)] = Nil
                                 ) {
  val request = new XMLHttpRequest

  def open(): Unit = request.open(method.name, urlDesr)

  def send(): Unit = request.send(JSON.stringify(an))

  def contentString(r: XMLHttpRequest): String = {
    request.response.toString
  }

  def responseJson(r: XMLHttpRequest): Unit = reponseConsumer.foreach(_ (JSON.parse(r.response.toString)))

  def responseRaw(r: XMLHttpRequest): Unit = reponseRawConsumer.foreach(_ (r.response.toString))

  def statusConsumer(r: Int): Unit = statusConsumer.foreach(consumer =>
    consumer(r)
  )

  def nonOkProcess(): Unit = DomShell.log(s"erreur sending $an to $urlDesr")

  def okStatus(status: Int): Boolean = method.okStatus(status)

  def prepare(): Unit = {

    open()
    val makeHeader: ((String, String)) => Unit = request.setRequestHeader _ tupled _
    //Envoie les informations du header adaptées avec la requête

    request.setRequestHeader("Content-Type", "application/json");
    headers.foreach(makeHeader)

    request.onreadystatechange = (e: Event) => { //Appelle une fonction au changement d'état.
      if (request.readyState == XMLHttpRequest.DONE) {
        DomShell.deb()
        statusConsumer(request.status)
        if (okStatus(request.status)) {
          okProcess.foreach(con => con())
          responseJson(request)
          responseRaw(request)
        } else {
          nonOkProcess()
        }
      }
    }

  }
}