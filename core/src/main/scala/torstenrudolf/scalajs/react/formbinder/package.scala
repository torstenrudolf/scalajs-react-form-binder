package torstenrudolf.scalajs.react
import scala.language.experimental.macros

package object formbinder {

  def bind[DataModel](formLayout: FormLayout[DataModel], validatorObject: Any): Form[DataModel] = macro Macros.generate[DataModel]

}
