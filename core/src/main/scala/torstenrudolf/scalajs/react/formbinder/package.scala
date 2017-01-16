package torstenrudolf.scalajs.react
import scala.language.experimental.macros

package object formbinder {

  def bindWithDefault[DataModel](formLayout: FormLayout[DataModel],
                                 validatorObject: Any,
                                 defaultModelValue: DataModel): Form[DataModel] = macro Macros.generateWithDefault[DataModel]

  def bind[DataModel](formLayout: FormLayout[DataModel],
                      validatorObject: Any): Form[DataModel] = macro Macros.generateWithoutDefault[DataModel]

//  def bind[DataModel](formLayout: FormLayout[DataModel],
//                      validatorObject: Any): Form[DataModel] = bind[DataModel](formLayout, validatorObject, None)

}
