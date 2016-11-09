package torstenrudolf.scalajs.react.formbinder

import japgolly.scalajs.react.{Callback, ReactNode}

import scala.scalajs.js

trait Form[DataModel] {
  def fullValidate: Callback

  def validatedFormData: Option[DataModel]

  def field[T](fd: FormFieldDescriptor[T]): ReactNode

  def globalValidationMessage: ReactNode

  def allFields: List[ReactNode]

  def setFieldValue[T](fd: FormFieldDescriptor[T], value: T): Callback

  def fieldValue[A](fd: FormFieldDescriptor[A]): Option[A]

  def clearField[A](fd: FormFieldDescriptor[A]): Callback

  def clearAllFields: Callback

  def resetFieldToDefault[A](fd: FormFieldDescriptor[A]): Callback

  def resetAllFields: Callback

}


sealed trait FormFailure

case object FormUninitialized extends Throwable("Uninitialized Field") with FormFailure

case object FormProcessingFailure extends Throwable("Form is actively processing") with FormFailure


case class FormFieldArgs[T](fieldName: String,
                            currentValue: Option[T],
                            currentValidationResult: ValidationResult,
                            onChangeCB: (T) => Callback,
                            resetToDefaultCB: Callback,
                            clearCB: Callback,
                            private val parentForm: Form[_] with FormAPI[_]) {

  def otherFieldValue[T2](fd: FormFieldDescriptor[T2]): Option[T2] = parentForm.fieldValue(fd)

  def clearOtherField[T2](fd: FormFieldDescriptor[T2]): Callback = parentForm.clearField(fd)

  def errorMessage: js.UndefOr[String] =
    if (currentValidationResult.isValid) js.undefined
    else currentValidationResult.errorMessage

}

case class FormFieldDescriptor[T](descr: (FormFieldArgs[T]) => ReactNode)


abstract class FormLayout[T] {

  def onChange(validatedData: Option[T],
               allFieldValidationResults: List[ValidationResult],
               globalFormValidationResult: ValidationResult): Callback = Callback.empty

}
