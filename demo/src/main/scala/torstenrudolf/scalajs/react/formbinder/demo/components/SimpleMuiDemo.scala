package torstenrudolf.scalajs.react.formbinder.demo.components

import chandu0101.macros.tojs.GhPagesMacros


object SimpleMuiDemo {
  val code = GhPagesMacros.exampleSource

  // EXAMPLE:START
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.vdom.prefix_<^._
  import chandu0101.scalajs.react.components.materialui.{MuiRaisedButton, MuiTextField}
  import chandu0101.scalajs.react.components.Implicits._
  import torstenrudolf.scalajs.react.formbinder._

  // the data model
  case class Data(username: String, password: String)

  // define validation rules in separate object -- note: the method names and signatures must match to the data model's
  object DataValidation {
    def username(username: String) = Validator(username.nonEmpty, "Please specify username")
    def password(password: String) = Validator(password.nonEmpty, "Please specify password")
  }

  case class State(data: Option[Data] = None)

  class Backend($: BackendScope[Unit, State]) {

    // then define the form layout fields -- names must match the data model's field names
    object FormLayout extends FormLayout[Data] {
      import torstenrudolf.scalajs.react.formbinder.materialui.FormFieldDescriptors._
      val username = MuiTextField(floatingLabelText = "Username").asFormFieldDescriptor  //TextField("Username")
      val password = MuiTextField(floatingLabelText = "Password", `type` = "password").asFormFieldDescriptor

      // define what to do after form data or form validation changes
      def onChange(validatedData: Option[Data],
                   allFieldValidationResults: List[ValidationResult],
                   globalFormValidationResult: ValidationResult): Callback = $.modState(_.copy(data = validatedData))
    }

    // this does the magic and binds the data model, validation rules and formlayout together
    val form = bind[Data](DataValidation, FormLayout)

    // use it like this:
    val handleSubmit: Callback = $.state >>= {
      _.data match {
        case Some(data) => Callback.alert(s"do what you need to do with $data")
        case None => form.showUninitializedFieldErrors
      }
    }

    // you have full control over the display of the form fields
    def render(state: State) = CodeExample(code, "SimpleFormDemo")(
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
          <.div(MuiRaisedButton(label = "Submit", onClick = (e: ReactEventH) => handleSubmit)())
        )
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