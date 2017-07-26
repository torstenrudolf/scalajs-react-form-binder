package torstenrudolf.scalajs.react.formbinder.demo.components

import chandu0101.macros.tojs.GhPagesMacros


object FormBinderTraitDemo {
  val code = GhPagesMacros.exampleSource

  // EXAMPLE:START
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.vdom.prefix_<^._
  import chandu0101.scalajs.react.components.materialui.{MuiRaisedButton, MuiTextField, MuiFlatButton}
  import chandu0101.scalajs.react.components.Implicits._
  import torstenrudolf.scalajs.react.formbinder._

  class Backend($: BackendScope[Unit, Unit]) {

    object myForm extends FormBinder {
      case class DataModel(username: String = "Joe", password: String, age: Int)

      // define validation rules in separate object -- note: the method names and signatures must match to the data model's
      object validatorsHolder {
        def username(username: String) = Validator(username.nonEmpty, "Please specify username")

        def password(password: String) = Validator(password.nonEmpty, "Please specify password")

        def age(age: Int) = Validator(age >= 18, "Must be at least 18")

        def $global(data: DataModel): ValidationResult = Validator(data.username != data.password || data.age > 32, "username and password must be different or age must be greater than 32.")

      }

      object formLayout extends FormLayout[DataModel] {

        import torstenrudolf.scalajs.react.formbinder.materialui.FormFieldDescriptors._

        val username = MuiTextField(floatingLabelText = "Username").asFormFieldDescriptor
        val password = MuiTextField(floatingLabelText = "Password", `type` = "password").asFormFieldDescriptor
        val age = MuiTextField(floatingLabelText = "Age", `type` = "number").asIntFormFieldDescriptor
      }

      val defaultFormValue = None

      // annoyingly we have to write this
      val form: Form[DataModel] = formX

      def handleSubmit[T](e: ReactEvent): Callback = {
        e.preventDefault()
        form.validatedFormData match {
          case Some(data) => Callback.alert(s"do what you need to do with $data")
          case _ => Callback.empty
        }
      }
    }

    // you have full control over the display of the form fields
    def render() = {
      CodeExample(code, "Using FormBinder trait")(
        <.div(
          <.div(
            <.h4("Using FormBinder trait"),
            <.form(
              ^.onSubmit ==> {(e: ReactEvent) => myForm.handleSubmit(e)},
              <.div(
                ^.display.flex,
                ^.flexDirection.column,
                ^.marginBottom := 15.px,
                myForm.form.field(myForm.formLayout.username),
                myForm.form.field(myForm.formLayout.password),
                myForm.form.field(myForm.formLayout.age)
              ),
              <.div(
                MuiFlatButton(label = "Clear", onClick = (e: ReactEventH) => myForm.form.clearAllFields)(),
                MuiFlatButton(label = "Reset", onClick = (e: ReactEventH) => myForm.form.resetAllFields)(),
                MuiRaisedButton(label = "Submit", `type` = "submit")()
              ),
              <.div(^.color := "red")(myForm.form.globalValidationResult.map(_.errorMessage))
            )
          )
        )
      )
    }
  }

  // EXAMPLE:END

  val component = ReactComponentB[Unit]("FormBinderTraitDemo")
    .stateless
    .renderBackend[Backend]
    .componentDidMount(scope =>
      Callback {
        scope.backend.myForm.form.subscribeToUpdates(scope.forceUpdate)
      } >> scope.forceUpdate
    )
    .build

  def apply() = component()

}