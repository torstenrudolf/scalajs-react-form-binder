package torstenrudolf.scalajs.react.formbinder.materialui

import chandu0101.scalajs.react.components.CssProperties
import chandu0101.scalajs.react.components.materialui._
import chandu0101.scalajs.react.components.Implicits._
import japgolly.scalajs.react._
import scala.scalajs.js.JSConverters._
import torstenrudolf.scalajs.react.formbinder._

/*
Some convenient helpers that reduce the boilerplate in simple cases
 */
object FormFieldDescriptors {

  implicit def _muiTextFieldExt(mf: MuiTextField): MuiTextFieldExt = new MuiTextFieldExt(mf)

  implicit def _muiSelectFieldExt[T](mf: MuiSelectField[T]): MuiSelectFieldExt[T] = new MuiSelectFieldExt[T](mf)

  implicit def _muiSliderFieldExt(mf: MuiSlider): MuiSliderFieldExt = new MuiSliderFieldExt(mf)

  final class MuiTextFieldExt(mf: MuiTextField) {
    require(mf.onChange.isEmpty && mf.errorText.isEmpty && mf.value.isEmpty)

    // would need a macro to do this so the types match?
    //    def asFormFieldDescriptor = {
    //      mf.`type`.getOrElse("text") match {
    //        case "number" => asNumberFormFieldDescriptor
    //        case _ => asTextFormFieldDescriptor
    //      }
    //    }

    def asFormFieldDescriptor: FormFieldDescriptor[String] = {
      FormFieldDescriptor[String]((a: FormFieldArgs[String]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onChange = (e: ReactEventI) => a.onChangeCB(e.target.value),
          errorText = a.currentValidationResult.errorMessage
        )()
      )
    }

    def asNumberFormFieldDescriptor: FormFieldDescriptor[Double] = {
      require(mf.`type`.get == "number")
      FormFieldDescriptor[Double]((a: FormFieldArgs[Double]) =>
        mf.copy(
          value = a.currentValue.map(_.toString).orUndefined,
          onChange = (e: ReactEventI) => a.onChangeCB(e.target.value.toDouble),
          errorText = a.currentValidationResult.errorMessage
        )()
      )
    }
  }

  final class MuiSelectFieldExt[T](mf: MuiSelectField[T]) {
    require(mf.onChange.isEmpty && mf.errorText.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor: FormFieldDescriptor[T] = {
      FormFieldDescriptor[T]((a: FormFieldArgs[T]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onChange = (e: ReactEventI, idx: Int, value: T) => a.onChangeCB(value),
          errorText = a.currentValidationResult.errorMessage
        )()
      )
    }
  }

  final class MuiSliderFieldExt(mf: MuiSlider) {
    require(mf.onChange.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor: FormFieldDescriptor[Double] = {
      FormFieldDescriptor[Double]((a: FormFieldArgs[Double]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onChange = (e: ReactEventH, value: Double) => a.onChangeCB(value),
          error = a.currentValidationResult.errorMessage
        )()
      )
    }
  }

}
