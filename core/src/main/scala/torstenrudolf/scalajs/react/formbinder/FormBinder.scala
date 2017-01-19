package torstenrudolf.scalajs.react.formbinder
import scala.language.experimental.macros
import scala.annotation.compileTimeOnly

trait FormBinder {
  type DataModel

  val formLayout: FormLayout[DataModel]

  val validatorsHolder: Any

  val defaultFormValue: Option[DataModel]

  protected def formX: Form[DataModel] = macro Macros.generateOnFormBinder[DataModel]

  // you need to override this with val form = formX in your implementation, macro expansion cannot happen here ;-(
  // todo: maybe this can be avoided by using macro annotation?
  val form: Form[DataModel]
}
