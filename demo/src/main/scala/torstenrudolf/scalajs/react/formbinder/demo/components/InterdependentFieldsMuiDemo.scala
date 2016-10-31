package torstenrudolf.scalajs.react.formbinder.demo.components

import chandu0101.macros.tojs.GhPagesMacros
import chandu0101.scalajs.react.components.materialui.{MuiMenuItem, MuiSelectField}


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
  case class Data(country: Country, province: Province)

  // define validation rules in separate object -- note: the method names and signatures must match to the data model's
  object DataValidation {
    def province(province: Province, country: Option[Country]) =
      Validator(country.forall(_.provinces.contains(province)), "Please select a province matching the country")
  }

  case class State(data: Option[Data] = None, globalValidationResult: Option[ValidationResult] = None)

  class Backend($: BackendScope[Unit, State]) {

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
    def render(state: State) = CodeExample(code, "Dependent Fields")(
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
          <.div(MuiRaisedButton(label = "Submit", onClick = (e: ReactEventH) => handleSubmit)())
        )
      )
    )
  }

  // EXAMPLE:END

  val component = ReactComponentB[Unit]("InterdependentFieldsMuiDemo")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = component()

}