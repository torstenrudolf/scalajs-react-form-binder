package torstenrudolf.scalajs.react.formbinder

object Validation {}


case class ValidationResult(errors: List[ValidationError]) {
  def isValid: Boolean = errors.isEmpty

  def errorMessage: String = errors.map(_.message).mkString(", ")

  def errorMessages: List[String] = errors.map(_.message)

  def ++(other: ValidationResult): ValidationResult =
    ValidationResult(this.errors ::: other.errors)

  def +(error: ValidationError): ValidationResult = ValidationResult(this.errors :+ error)

}

object ValidationResult {

  def Success = ValidationResult(Nil)

  def withError(message: String) = ValidationResult(List(ValidationError(message)))

}

case class ValidationError(message: String)

object Validator {

  def apply(p: Boolean, message: String) = {
    if (!p) ValidationResult.withError(message)
    else ValidationResult.Success
  }
}