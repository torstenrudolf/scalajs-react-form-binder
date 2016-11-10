package torstenrudolf.scalajs.react.formbinder.demo.components

import chandu0101.macros.tojs.GhPagesMacros
import chandu0101.scalajs.react.components.materialui.{MuiFlatButton, MuiMenuItem, MuiSelectField}


object InterdependentFieldsMuiDemo {
  val code = GhPagesMacros.exampleSource

  // EXAMPLE:START
  import scala.scalajs.js.JSConverters._
  import chandu0101.scalajs.react.components.Implicits._
  import chandu0101.scalajs.react.components.materialui.{MuiRaisedButton, MuiTextField}
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.vdom.prefix_<^._
  import torstenrudolf.scalajs.react.formbinder._

  abstract class Country(val name: String, val provinces: List[Province])
  case object Australia extends Country("Australia", List(Victoria, Queensland))
  case object Germany extends Country("Germany", List(Bayern, Sachsen))
  val countries = List(Australia, Germany)

  abstract class Province(val name: String)
  case object Victoria extends Province("Victoria")
  case object Queensland extends Province("Queensland")
  case object Bayern extends Province("Bayern")
  case object Sachsen extends Province("Sachsen")

  // the data model
  case class Data(country: Country = Australia, province: Province)

  // define validation rules in separate object -- note: the method names and signatures must match to the data model's
  object DataValidation {
    def province(province: Province, country: Option[Country]) =
      Validator(country.forall(_.provinces.contains(province)), "Please select a province matching the country")
  }

  // then define the form layout fields -- names must match the data model's field names
  object FormLayout extends FormLayout[Data] {
    val country: FormFieldDescriptor[Country] = FormFieldDescriptor((a: FormFieldArgs[Country]) =>
      MuiSelectField[Country](
        floatingLabelText = "Country",
        onChange = (e: ReactEventI, idx: Int, value: Country) => a.onChangeCB(value) >> a.clearOtherField(province),
        value = a.currentValue.orUndefined,
        errorText = a.currentValidationResult.errorMessage
      )(
        countries.map(country => MuiMenuItem[Country](key = country.name, value = country, primaryText = country.name)())
      )
    )
    // need to write this descriptor by hand to have dynamic change of the selectable items
    val province = FormFieldDescriptor((a: FormFieldArgs[Province]) =>
      MuiSelectField[Province](
        floatingLabelText = "Province/State",
        onChange = (e: ReactEventI, idx: Int, value: Province) => a.onChangeCB(value),
        value = a.currentValue.orUndefined,
        errorText = a.currentValidationResult.errorMessage
      )(
        a.otherFieldValue(country).toList.flatMap(_.provinces.map(province =>
          MuiMenuItem[Province](key = province.name, value = province, primaryText = province.name)()
        ))
      )
    )
  }

  // this does the magic and binds the data model, validation rules and formlayout together
  val form = bind[Data](FormLayout, DataValidation)

  class Backend($: BackendScope[Unit, Unit]) {

    // use it like this:
    def handleSubmit: Callback = {
      form.validatedFormData match {
        case Some(data) => Callback.alert(s"do what you need to do with $data")
        case _ => Callback.empty
      }
    }

    // you have full control over the display of the form fields
    def render() = CodeExample(code, "Dependent Fields")(
      <.div(
        <.form(
          ^.onSubmit --> handleSubmit,
          <.div(
            ^.display.flex,
            ^.flexDirection.column,
            ^.marginBottom := 15.px,
            form.field(FormLayout.country),
            form.field(FormLayout.province)
          ),
          <.div(
            MuiFlatButton(label = "Clear", onClick = (e: ReactEventH) => form.clearAllFields)(),
            MuiFlatButton(label = "Reset", onClick = (e: ReactEventH) => form.resetAllFields)(),
            MuiRaisedButton(label = "Submit", onClick = (e: ReactEventH) => handleSubmit)()
          ),
          <.div(^.color := "red")(form.globalValidationMessage)
        )
      )
    )
  }

  // EXAMPLE:END

  val component = ReactComponentB[Unit]("InterdependentFieldsMuiDemo")
    .stateless
    .renderBackend[Backend]
    .build

  def apply() = component()

}