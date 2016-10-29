package torstenrudolf.scalajs.react.formbinder

import japgolly.scalajs.react.{Callback, ReactNode}

trait Form[DataModel] {

  def allFields: List[ReactNode]

  def field[T](fd: FormFieldDescriptor[T]): ReactNode

  def setFieldValue[T](fd: FormFieldDescriptor[T], value: T): Callback

  def showUninitializedFieldErrors: Callback

  def fieldValue[A](fd: FormFieldDescriptor[A]): Option[A]

}


sealed trait FormFailure

case object FormUninitialized extends Throwable("Uninitialized Field") with FormFailure

case object FormProcessingFailure extends Throwable("Form is actively processing") with FormFailure


case class FormFieldArgs[T](fieldName: String,
                            currentValue: Option[T],
                            currentValidationResult: ValidationResult,
                            onChangeCB: (T) => Callback,
                            // todo: Do they need to be functions?
                            resetCB: () => Callback, // sets to defaultValue from DataModel if present
                            clearCB: () => Callback,
                            private val parentForm: Form[_] with FormAPI[_]) {

  def otherFieldValue[T2](fd: FormFieldDescriptor[T2]): Option[T2] = parentForm.fieldValue(fd)

  def clearOtherField[T2](fd: FormFieldDescriptor[T2]): Callback = parentForm.fieldBinding(fd).clear()
}

case class FormFieldDescriptor[T](descr: (FormFieldArgs[T]) => ReactNode)


abstract class FormLayout[T] {
  /*

   */
  def onChange(newData: Option[T],
               allFieldValidationResults: List[ValidationResult],
               globalFormValidationResult: ValidationResult): Callback

}
