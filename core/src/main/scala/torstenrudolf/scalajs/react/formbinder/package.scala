package torstenrudolf.scalajs.react
import scala.language.experimental.macros

package object formbinder {

  // not: currently scala macros dont support default values for arguments

  def bindWithDefault[DataModel](formLayout: FormLayout[DataModel],
                                 validatorObject: Any,
                                 defaultModelValue: DataModel): Form[DataModel] = macro Macros.generateWithDefault[DataModel]

  def bind[DataModel](formLayout: FormLayout[DataModel],
                      validatorObject: Any): Form[DataModel] = macro Macros.generateWithoutDefault[DataModel]

}
