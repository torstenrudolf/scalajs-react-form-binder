package torstenrudolf.scalajs.react.formbinder.materialui

import chandu0101.scalajs.react.components.CssProperties
import chandu0101.scalajs.react.components.materialui._
import chandu0101.scalajs.react.components.Implicits._
import japgolly.scalajs.react._

import scala.scalajs.js.JSConverters._
import torstenrudolf.scalajs.react.formbinder._

import scala.scalajs.js
import scala.util.Try

/*
Some convenient helpers that reduce the boilerplate in simple cases
 */
object FormFieldDescriptors {

  implicit class MuiTextFieldExt(mf: MuiTextField) {
    require(mf.onChange.isEmpty && mf.errorText.isEmpty && mf.value.isEmpty)

    // would need a macro to do this so the types match?
    //    def asFormFieldDescriptor = {
    //      mf.`type`.getOrElse("text") match {
    //        case "number" => asDoubleFormFieldDescriptor
    //        case _ => asTextFormFieldDescriptor
    //      }
    //    }

    def asFormFieldDescriptor: FormFieldDescriptor[String] = {
      FormFieldDescriptor[String]((a: FormFieldArgs[String]) =>
        mf.copy(
          value = a.currentValue.getOrElse("").asInstanceOf[js.UndefOr[String]],
          onChange = (e: ReactEventI) => a.onChangeCB(e.target.value),
          errorText = a.errorMessage.map(_.asInstanceOf[ReactNode])
        )()
      )
    }

    def asDoubleFormFieldDescriptor: FormFieldDescriptor[Double] = {
      require(mf.`type`.get == "number")
      FormFieldDescriptor[Double]((a: FormFieldArgs[Double]) =>
        mf.copy(
          value = a.currentValue.getOrElse(0).toString,
          onChange = (e: ReactEventI) => a.onChangeCB(Try(e.target.value.toDouble).getOrElse(0)),
          errorText = a.errorMessage.map(_.asInstanceOf[ReactNode])
        )()
      )
    }


    def asIntFormFieldDescriptor: FormFieldDescriptor[Int] = {
      require(mf.`type`.get == "number")
      FormFieldDescriptor[Int]((a: FormFieldArgs[Int]) =>
        mf.copy(
          value = a.currentValue.getOrElse(0).toString,
          onChange = (e: ReactEventI) => a.onChangeCB(Try(e.target.value.toInt).getOrElse(0)),
          errorText = a.errorMessage.map(_.asInstanceOf[ReactNode])
        )()
      )
    }
  }

  implicit class MuiSelectFieldExt[T](mf: MuiSelectField[T]) {
    require(mf.onChange.isEmpty && mf.errorText.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor(childNodes: ReactNode*): FormFieldDescriptor[T] = {
      FormFieldDescriptor[T]((a: FormFieldArgs[T]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onChange = (e: ReactEventI, idx: Int, value: T) => a.onChangeCB(value),
          errorText = a.errorMessage.map(_.asInstanceOf[ReactNode])
        )(childNodes)
      )
    }
  }

  implicit class MuiSliderFieldExt(mf: MuiSlider) {
    require(mf.onChange.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor: FormFieldDescriptor[Double] = {
      FormFieldDescriptor[Double]((a: FormFieldArgs[Double]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onChange = (e: ReactEventH, value: Double) => a.onChangeCB(value),
          error = a.errorMessage
        )()
      )
    }
  }

  implicit class MuiCheckboxExt[T](mf: MuiCheckbox[T]) {
    // note: checkbox has no errorText
    require(mf.onCheck.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor: FormFieldDescriptor[Boolean] = {
      FormFieldDescriptor[Boolean]((a: FormFieldArgs[Boolean]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onCheck = (e: ReactEventH, checked: Boolean) => a.onChangeCB(checked)
        )()
      )
    }
  }

  implicit class MuiToggleExt[T](mf: MuiToggle[T]) {
    // note: checkbox has no errorText
    require(mf.onToggle.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor: FormFieldDescriptor[Boolean] = {
      FormFieldDescriptor[Boolean]((a: FormFieldArgs[Boolean]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onToggle = (e: ReactEventI, checked: Boolean) => a.onChangeCB(checked)
        )()
      )
    }
  }

  implicit class MuiRadioButtonGroupExt(mf: MuiRadioButtonGroup[String]) {
    // MuiRadioButtonGroup.onChange only supplies String instead of T, so I implement it only for string for now
    require(mf.onChange.isEmpty && mf.value.isEmpty)

    def asFormFieldDescriptor: FormFieldDescriptor[String] = {
      FormFieldDescriptor[String]((a: FormFieldArgs[String]) =>
        mf.copy(
          value = a.currentValue.orUndefined,
          onChange = (e: ReactEventI, value: String) => a.onChangeCB(value)
        )()
      )
    }
  }

}
