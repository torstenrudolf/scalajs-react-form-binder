package torstenrudolf.scalajs.react.formbinder

import japgolly.scalajs.react.{Callback, ReactNode}

import scala.scalajs.js

trait Form[DataModel] {
  def fullValidate: Callback

  def validatedFormData: Option[DataModel]

  // todo: clean up and sync with `validatedFormData`
  def validatedFormDataNew(reValidate: Boolean): Option[DataModel]

  def field[T](fd: FormFieldDescriptor[T]): ReactNode

  def globalValidationResult: Option[ValidationResult]

  def allFields: List[ReactNode]

  def setModelValue(newModelValue: DataModel): Callback

  def setFieldValue[T](fd: FormFieldDescriptor[T], value: T): Callback

  def fieldValue[A](fd: FormFieldDescriptor[A]): Option[A]

  def clearField[A](fd: FormFieldDescriptor[A]): Callback

  def clearAllFields: Callback

  def resetFieldToDefault[A](fd: FormFieldDescriptor[A]): Callback

  def resetAllFields: Callback

  def subscribeToUpdates(cb: Callback): Unit

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
                            private[formbinder] val parentForm: Form[_] with FormAPI[_]) {

  def otherFieldValue[T2](fd: FormFieldDescriptor[T2]): Option[T2] = parentForm.fieldValue(fd)

  def clearOtherField[T2](fd: FormFieldDescriptor[T2]): Callback = parentForm.clearField(fd)

  def errorMessage: js.UndefOr[String] =
    if (currentValidationResult.isValid) js.undefined
    else currentValidationResult.errorMessage


  def transform[B](f1: B => T, f2: T => Option[B]): FormFieldArgs[B] = {
    FormFieldArgs[B](
      fieldName = this.fieldName,
      currentValue = this.currentValue.flatMap(f2),
      currentValidationResult = this.currentValidationResult,
      onChangeCB = (b: B) => this.onChangeCB(f1(b)),
      resetToDefaultCB = this.resetToDefaultCB,
      clearCB = this.clearCB,
      parentForm: Form[_] with FormAPI[_]
    )
  }

}


// todo: this could just be a type alias
case class FormFieldDescriptor[T](descr: (FormFieldArgs[T]) => ReactNode)


abstract class FormLayout[T] {

  // a hook if you need to react on form changes
  //  this gets triggered every time a field's unvalidated value changes
  def onChange(validatedData: Option[T]): Callback = Callback.empty

}
