package torstenrudolf.scalajs.react.formbinder.demo

import chandu0101.scalajs.react.components.Implicits._
import chandu0101.scalajs.react.components.materialui._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.{Resolution, RouterCtl}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import org.scalajs.dom.UIEvent
import torstenrudolf.scalajs.react.formbinder.demo.AppRouter.{AppPage, MenuItem}

import scala.scalajs.js


object PageWrapper {

  import WithWidth._

  val baseTheme = Mui.Styles.LightRawTheme
  val backgroundColor: js.UndefOr[MuiColor] = js.undefined


  val materialTheme = Mui.Styles.getMuiTheme(backgroundColor.fold(baseTheme)(
    color => baseTheme.copy(palette = baseTheme.palette.copy(canvasColor = color)))
  )

  case class Props(menuItems: Vector[MenuItem],
                   currentRouteResolution: Resolution[AppPage],
                   routerCtrl: RouterCtl[AppPage])

  case class State(screenWidth: Width = Small,
                   navDrawerOpen: Boolean = false,
                   snackbarOpen: Boolean = false,
                   snackbarMessage: ReactNode = "") {
    def navDrawerToggled = this.copy(navDrawerOpen = !this.navDrawerOpen)

  }

  case class Backend($: BackendScope[Props, State]) {
    def toggleDrawerCB = $.modState(_.navDrawerToggled)

    def showMenuAsDrawer(w: Width) = w != Large

    def render(p: Props, s: State) = {
      val title = s"scalajs-react-formbinder demo"

      WithWidth(newWidth => $.modState(_.copy(screenWidth = newWidth)))(
        MuiMuiThemeProvider(key = "ThemeProvider")(// this enables us to use the material-ui components
          MuiPaper(key = "MainPaper")(
            <.div(
              <.titleTag(title),

              MuiPaper()(
                MuiAppBar(
                  key = "AppBar",
                  title = s"$title",
                  onLeftIconButtonTouchTap = (e: ReactTouchEventH) => toggleDrawerCB,
                  onTitleTouchTap = (e: ReactTouchEventH) => Callback.alert("onTitleTouchTap; we might want to redirect to landing page here"),
                  showMenuIconButton = showMenuAsDrawer(s.screenWidth),
                  iconElementRight = MuiIconButton(href = "https://github.com/torstenrudolf/scalajs-react-form-binder", iconClassName="muidocs-icon-custom-github")()
                )()
              ),

              <.div(^.display.flex, ^.minHeight:=600.px)(
                if (showMenuAsDrawer(s.screenWidth)) {
                  Seq(
                    MuiDrawer(
                      key = "MenuDrawer",
                      open = s.navDrawerOpen,
                      docked = false,
                      onRequestChange = (open: Boolean, reason: String) => toggleDrawerCB
                    )(
                      MuiMenu[AppPage](value = p.currentRouteResolution.page)(
                        //MuiSubheader(key="menu", inset=true)("Menu"), // maybe this should have username like in gmail

                        p.menuItems.map(m =>
                          MuiMenuItem(
                            value = m.page,
                            key = m.name,
                            onTouchTap = (e: ReactTouchEventH) => toggleDrawerCB >> p.routerCtrl.set(m.page)
                          )(m.name)
                        )
                      )
                    ),
                    <.div(p.currentRouteResolution.render()).render
                  )
                } else {
                  Seq(
                    <.div(^.width:=224.px, ^.borderRight := "1px solid rgb(223, 220, 220)")(
                      MuiMenu[AppPage](value = p.currentRouteResolution.page)(
                        p.menuItems.map(m =>
                          MuiMenuItem(
                            value = m.page,
                            key = m.name,
                            onTouchTap = (e: ReactTouchEventH) => p.routerCtrl.set(m.page)
                          )(m.name)
                        )
                      )
                    ),
                    <.div(p.currentRouteResolution.render())
                  )
                }
              )
            )
          )
        )
      )
    }
  }

  val component = ReactComponentB[Props]("PageWrapper")
    .initialState(State())
    .renderBackend[Backend]
    .build


  def apply(menuItems: Vector[MenuItem],
            currentRouteResolution: Resolution[AppPage],
            routerCtrl: RouterCtl[AppPage],
            key: js.UndefOr[js.Any] = js.undefined,
            ref: js.UndefOr[String] = js.undefined): ReactElement =
    component.set(key, ref)(Props(menuItems, currentRouteResolution, routerCtrl))

}


object WithWidth {

  // see https://github.com/callemall/material-ui/blob/master/src/utils/withWidth.js


  sealed trait Width {
    val minSize: Double
  }

  object Width {
    def sizeToWidth(width: Double): Width = width match {
      case w if w >= Large.minSize => Large
      case w if w >= Medium.minSize => Medium
      case _ => Small
    }
  }

  case object Small extends Width {
    val minSize = 0.0
  }

  case object Medium extends Width {
    val minSize = 768.0
  }

  case object Large extends Width {
    val minSize = 992.0
  }

  val resizeInterval = 166

  // in the original version they have a timer with this interval (Corresponds to 10 frames at 60 Hz.)

  case class Props(childNode: ReactNode, onResizeCB: Width => Callback)

  case class State(width: Width = Small)

  case class Backend($: BackendScope[Props, State]) {

    def updateWidth(p: Props): Callback = {
      val width = Width.sizeToWidth(dom.window.innerWidth)
      $.modState(_.copy(width = width)) >> p.onResizeCB(width)
    }

    def render(p: Props) = p.childNode.asInstanceOf[ReactComponentU_]

  }


  private val component = ReactComponentB[Props]("WithWidth")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.updateWidth(scope.props) >> Callback {
      dom.window.addEventListener[UIEvent]("resize", (e: UIEvent) => scope.backend.updateWidth(scope.props).runNow())
    })
    .componentWillUnmount(scope => Callback {
      dom.window.removeEventListener[UIEvent]("resize", (e: UIEvent) => scope.backend.updateWidth(scope.props).runNow())
    })
    .build


  def apply(onResizeCB: Width => Callback)(childNode: ReactNode) = component(Props(childNode, onResizeCB))
}