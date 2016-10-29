package torstenrudolf.scalajs.react
import scala.language.experimental.macros

package object formbinder {

  def bind[T](validatorObject: Any, formLayout: FormLayout[T]): Form[T] = macro Macros.generate[T] //Layout with Form[T] with FormProcs[T] = macro Macro.generate[T, Layout]

}
