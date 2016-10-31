package torstenrudolf.scalajs.react
import scala.language.experimental.macros

package object formbinder {

  def bind[DataModel](validatorObject: Any, formLayout: FormLayout[DataModel]): Form[DataModel] = macro Macros.generate[DataModel] //Layout with Form[T] with FormProcs[T] = macro Macro.generate[T, Layout]

}
