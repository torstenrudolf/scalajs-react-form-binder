package torstenrudolf.scalajs.react.formbinder.demo.components

import chandu0101.macros.tojs.GhPagesMacros


object SimpleMuiDemo {
  val code = GhPagesMacros.exampleSource

  // EXAMPLE:START
  import scala.scalajs.js
  import js.JSConverters._
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.vdom.prefix_<^._
  import chandu0101.scalajs.react.components.materialui.{MuiRaisedButton, MuiTextField, MuiFlatButton}
  import chandu0101.scalajs.react.components.Implicits._
  import torstenrudolf.scalajs.react.formbinder._

  // the data model
  case class Data(username: String = "Joe", password: String, age: Int)

  // define validation rules in separate object -- note: the method names and signatures must match to the data model's
  object DataValidation {
    def username(username: String) = Validator(username.nonEmpty, "Please specify username")

    def password(password: String) = Validator(password.nonEmpty, "Please specify password")

    def age(age: Int) = Validator(age >= 18, "Must be at least 18")

    // specify global validation rules
    def $global(data: Data) = Validator(data.username != data.password, "Username and password must be different.")
  }

  // then define the form layout fields -- names must match the data model's field names
  object FormLayout extends FormLayout[Data] {

    import torstenrudolf.scalajs.react.formbinder.materialui.FormFieldDescriptors._

    val username = MuiTextField(floatingLabelText = "Username").asFormFieldDescriptor
    val password = MuiTextField(floatingLabelText = "Password", `type` = "password").asFormFieldDescriptor
    val age = MuiTextField(floatingLabelText = "Age", `type` = "number").asIntFormFieldDescriptor

    val calculatedField = FormFieldDescriptor[Unit]((a: FormFieldArgs[Unit]) =>
      MuiTextField(floatingLabelText = "Calculated Field", disabled = true, value = (a.otherFieldValue(username) zip a.otherFieldValue(age)).map{case (un, age) => s"$un ($age yrs)"}.headOption.orUndefined)()
    )
  }

  class Backend($: BackendScope[Unit, Unit]) {

    // this does the magic and binds the data model, validation rules and formlayout together
    val form = bind[Data](FormLayout, DataValidation)

    val form2 = bindWithDefault[Data](FormLayout, DataValidation, Data("heinz", "you won't know", 91))

    // use it like this:
    def handleSubmit[T](e: ReactEvent, f: Form[T]): Callback = {
      e.preventDefault()
      f.validatedFormData match {
        case Some(data) => Callback.alert(s"do what you need to do with $data")
        case _ => Callback.empty
      }
    }

    def overrideFormData(form: Form[Data]): Callback = {
      val randInt = (100 * js.Math.random()).toInt
      form.setModelValue(Data(username = s"John$randInt", password = s"top secret $randInt", age = randInt))
    }

    // you have full control over the display of the form fields
    def render() = {
      println(s"form: ${form.globalValidationResult}; form2: ${form2.globalValidationResult}")
      CodeExample(code, "Using Material-UI")(
        <.div(
          <.div(
            <.h4("With default values from Data case class"),
            <.form(
              ^.onSubmit ==> {(e: ReactEvent) => handleSubmit(e, form)},
              <.div(
                ^.display.flex,
                ^.flexDirection.column,
                ^.marginBottom := 15.px,
                form.allFields
              ),
              <.div(
                MuiFlatButton(label = "Override", onClick = (e: ReactEventH) => overrideFormData(form))(),
                MuiFlatButton(label = "Clear", onClick = (e: ReactEventH) => form.clearAllFields)(),
                MuiFlatButton(label = "Reset", onClick = (e: ReactEventH) => form.resetAllFields)(),
                MuiRaisedButton(label = "Submit", `type` = "submit")()
              ),
              <.div(^.color := "red")(form.globalValidationResult.map(_.errorMessage))
            )
          ),


          <.div(
            <.h4("With explicit default values"),
            <.form(
              ^.onSubmit ==> {(e: ReactEvent) => handleSubmit(e, form2)},
              <.div(
                ^.display.flex,
                ^.flexDirection.column,
                ^.marginBottom := 15.px,
                form2.field(FormLayout.username),
                form2.field(FormLayout.password),
                form2.field(FormLayout.age)
              ),
              <.div(
                MuiFlatButton(label = "Override", onClick = (e: ReactEventH) => overrideFormData(form2))(),
                MuiFlatButton(label = "Clear", onClick = (e: ReactEventH) => form2.clearAllFields)(),
                MuiFlatButton(label = "Reset", onClick = (e: ReactEventH) => form2.resetAllFields)(),
                MuiRaisedButton(label = "Submit", `type` = "submit")()
              ),
              <.div(^.color := "red")(form2.globalValidationResult.map(_.errorMessage))
            )
          )
        )
      )
    }
  }

  // EXAMPLE:END

  val component = ReactComponentB[Unit]("SimpleMuiDemo")
    .stateless
    .renderBackend[Backend]
    .componentDidMount(scope =>
      Callback {
        scope.backend.form.subscribeToUpdates(scope.forceUpdate)
        scope.backend.form2.subscribeToUpdates(scope.forceUpdate)
      } >> scope.forceUpdate
    )
    .build

  def apply() = component()

}