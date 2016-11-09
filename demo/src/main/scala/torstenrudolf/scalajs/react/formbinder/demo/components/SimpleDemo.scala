package torstenrudolf.scalajs.react.formbinder.demo.components

import chandu0101.macros.tojs.GhPagesMacros


object SimpleDemo {
  val code = GhPagesMacros.exampleSource

  // EXAMPLE:START
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.vdom.prefix_<^._
  import torstenrudolf.scalajs.react.formbinder._

  // the data model
  case class Data(username: String, password: String)

  // define validation rules in separate object -- note: the method names and signatures must match to the data model's
  object DataValidation {
    def username(username: String): ValidationResult =
      if (username.isEmpty) ValidationResult.withError("Please specify username") else ValidationResult.Success

    // simpler syntax with `Validator` helper
    def password(password: String): ValidationResult = Validator(password.nonEmpty, "Please specify password")
  }

  // then define the form layout fields -- names must match the data model's field names
  object FormLayout extends FormLayout[Data] {
    val username = TextField("Username")
    val password = TextField("Password", isPassword = true)

    // the FormFieldDescriptor that defines error display and binds the onChangeCB to the field
    private def TextField(labelText: String, isPassword: Boolean = false) = {
      val tpe = if (isPassword) "password" else "text"
      FormFieldDescriptor((a: FormFieldArgs[String]) =>
        <.div(
          <.label(labelText),
          <.input(
            ^.onChange ==> { (e: ReactEventI) => a.onChangeCB(e.target.value) },
            ^.value := a.currentValue,
            ^.`type` := tpe
          ),
          !a.currentValidationResult.isValid ?= <.span(^.color:="red")(a.currentValidationResult.errorMessage)
        )
      )
    }
  }

  // this does the magic and binds the data model, validation rules and formlayout together
  val form = bind[Data](FormLayout, DataValidation)

  class Backend($: BackendScope[Unit, Unit]) {

    // use it like this:
    def handleSubmit: Callback = {
      form.fullValidate >> {
        form.validatedFormData match {
          case Some(data) => Callback.alert(s"do what you need to do with $data")
          case _ => Callback.empty
        }
      }
    }

    // you have full control over the display of the form fields
    def render() = CodeExample(code, "Simple Form")(
      <.div(
        <.form(
          ^.onSubmit --> handleSubmit,
          <.div(
            ^.display.flex,
            ^.flexDirection.column,
            ^.marginBottom := 15.px,
            form.field(FormLayout.username),
            form.field(FormLayout.password)
          ),
          <.div(
            <.button("Submit")
          ),
          <.div(^.color := "red")(form.globalValidationMessage)
        )
      )
    )
  }

  // EXAMPLE:END

  val component = ReactComponentB[Unit]("SimpleFormDemo")
    .stateless
    .renderBackend[Backend]
    .build

  def apply() = component()

}