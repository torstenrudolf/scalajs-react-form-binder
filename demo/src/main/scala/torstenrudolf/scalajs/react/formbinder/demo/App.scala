package torstenrudolf.scalajs.react.formbinder.demo

import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.extra.router._
import torstenrudolf.scalajs.react.formbinder.demo.components.{InterdependentFieldsMuiDemo, SimpleDemo, SimpleMuiDemo}


/**
  * This is the main entry point into our scalajs application.
  */
@JSExport
object App {

  @JSExport
  def main(containerElement: dom.Node): Unit = {
    AppRouter.router().render(containerElement)
  }

}


object AppRouter {

  trait AppPage

  case object Simple extends AppPage
  case object SimpleMui extends AppPage
  case object ComplexMui extends AppPage

  type AppRouterCtrl = RouterCtl[AppPage]

  val config = RouterConfigDsl[AppPage].buildConfig { dsl =>
    import dsl._

    (trimSlashes
      | staticRedirect(root) ~> redirectToPage(Simple)(Redirect.Replace)
      | staticRoute("#simple", Simple) ~> render(SimpleDemo())
      | staticRoute("#simple-mui", SimpleMui) ~> render(SimpleMuiDemo())
      | staticRoute("#complex-mui", ComplexMui) ~> render(InterdependentFieldsMuiDemo())
      )
      .notFound(redirectToPage(Simple)(Redirect.Replace))
      .verify(// ensure all pages are configured and valid, see: https://github.com/japgolly/scalajs-react/blob/master/doc/ROUTER.md#a-spot-of-unsafeness
        Simple, SimpleMui, ComplexMui
      )
      .renderWith(layout)
  }


  case class MenuItem(name: String, page: AppPage)

  def mainMenuItems = Vector(
    MenuItem("Simple", Simple),
    MenuItem("Material-UI", SimpleMui),
    MenuItem("Dependent Fields", ComplexMui)
  )

  def layout(c: RouterCtl[AppPage], r: Resolution[AppPage]) = {
    PageWrapper(
      menuItems = mainMenuItems,
      currentRouteResolution = r,
      routerCtrl = c)
  }

  val baseUrl = BaseUrl.until_#

  val router = Router(baseUrl, config)

}
