package bon.jo.app

import bon.jo.game.html.Template
import bon.jo.html.DomShell.{$, ExtendedHTMLCollection}
import bon.jo.html.util.Anim
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.Element

import scala.concurrent.Future
import scala.scalajs.js.Promise
import scala.scalajs.js.JSConverters._
import scala.concurrent.ExecutionContext.Implicits._

trait AppLoader {

  /**
   * get the conf for an app and inject html from body template in element.
   * When it's done, call afterInDom on the template
   *
   * @param app
   * @param element
   * @return
   */
  def loadApp(app: String, element: Element): HtmlApp[Template] = {
    val confo: HtmlAppFactory[Template] = conf(app).asInstanceOf[HtmlAppFactory[Template]]
    val htmlFact = confo.htmlAppFactory
    val templateFact = confo.templateFactory
    val template = templateFact()
    element.innerHTML = template.body
    val appDiv: Div = $(template.id)
    val ret = htmlFact(appDiv, template)
    template.init(appDiv)
    ret
  }


  /**
   * get the conf for an app and inject html from body template in element.
   * When it's done, call afterInDom on the template
   *
   * @param app
   * @param element
   * @return
   */
  def loadAppAsync(app: String, element: Element): Future[HtmlApp[Template]] = Future {
    val confo: HtmlAppFactory[Template] = conf(app).asInstanceOf[HtmlAppFactory[Template]]
    val htmlFact = confo.htmlAppFactory
    val templateFact = confo.templateFactory
    val template = templateFact()
    element.innerHTML = template.body
    val appDiv: Div = $(template.id)
    val ret = htmlFact(appDiv, template)
    template.init(appDiv)
    ret
  }

//  def authLoad(app: String, element: Element): Future[HtmlApp[Template]] = {
//    Auth.doAuth()
//  }
  /**
   * find the apps in html and load it
   *
   * @param apps
   */
  def loads(apps: List[String]): Unit = {
    for (app <- apps) {
      val appInit = document.getElementsByTagName(app)
      if (appInit != null && appInit.nonEmpty) {
        loadApp(app, appInit(0))
      }
    }
    Anim.start()
  }

//  /**
//   * find the apps in html and load it
//   *
//   * @param apps
//   */
//  def loadsAsync(apps: List[String]): List[Promise[HtmlApp[Template]]] = {
//    (for (app <- apps) yield {
//      val appInit = document.getElementsByTagName(app)
//      if (appInit != null && appInit.nonEmpty) {
//        Some(loadAppAsync(app, appInit(0)))
//      } else {
//        None
//      }
//    }).filter(_.isDefined).map(_.get)
//
//  }

  val conf: Map[String, HtmlAppFactory[_]]
  val apps: List[String]
}
