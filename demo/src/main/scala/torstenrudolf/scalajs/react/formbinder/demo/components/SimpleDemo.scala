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

  case class State(data: Option[Data] = None)

  class Backend($: BackendScope[Unit, State]) {

    // then define the form layout fields -- names must match the data model's field names
    object FormLayout extends FormLayout[Data] {
      val username = TextField("Username")
      val password = TextField("Password", isPassword = true)

      // define what to do after form data or form validation changes
      def onChange(validatedData: Option[Data],
                   allFieldValidationResults: List[ValidationResult],
                   globalFormValidationResult: ValidationResult): Callback = $.modState(_.copy(data = validatedData))
    }

    // this does the magic and binds the data model, validation rules and formlayout together
    val form = bind[Data](FormLayout, DataValidation)

    // use it like this:
    val handleSubmit: Callback = $.state >>= {
      _.data match {
        case Some(data) => Callback.alert(s"do what you need to do with $data")
        case None => form.showUninitializedFieldErrors
      }
    }

    // you have full control over the display of the form fields
    def render(state: State) = CodeExample(code, "Simple Form")(
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
          <.div(<.button("Submit"))
        )
      )
    )
  }

  // the FormFieldDescriptor that defines error display and binds the onChangeCB to the field
  def TextField(labelText: String, isPassword: Boolean = false) = {
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

  // EXAMPLE:END

  val component = ReactComponentB[Unit]("SimpleFormDemo")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = component()

}