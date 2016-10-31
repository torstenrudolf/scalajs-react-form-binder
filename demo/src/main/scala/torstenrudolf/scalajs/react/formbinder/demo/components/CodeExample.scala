package torstenrudolf.scalajs.react.formbinder.demo.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js


object CodeExample {

  /*
  shamelessly stolen from https://github.com/chandu0101/scalajs-react-components/blob/master/demo/src/main/scala/demo/components/CodeExample.scala
   */

  case class Backend($: BackendScope[Props, _]) {
    def render(props: Props, children: PropsChildren) = {
      <.div(
        props.title.nonEmpty ?= <.h3(props.title, ^.paddingBottom := "15px"),
        <.div(
          ^.borderRadius := "2px",
          ^.boxShadow := "0 1px 4px rgba(223, 228, 228, 0.79)",
          ^.maxWidth := "1024px"
        )(
          <.div(^.padding := "30px", ^.key := "dan")(children),
          <.pre(^.borderTop := "solid 1px #e0e0e0", ^.key := "code")(
            CodeHighlight(props.code)
          )
        )
      )
    }
  }

  val component = ReactComponentB[Props]("codeexample")
    .renderBackend[Backend]
    .build

  case class Props(code: String, title: String)

  def apply(code: String,
            title: String,
            ref: js.UndefOr[String] = "",
            key: js.Any = {})
           (children: ReactNode*) =
    component.set(key, ref)(Props(code, title), children: _*)
}
