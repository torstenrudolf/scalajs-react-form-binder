# FormBinder

FormBinder is a tool to bind reactjs form fields to a DataModel plus validation.


## Usage

```scala
import FormBinder.{ValidationResult, Validator}


case class Data(username: String = "", 
                password1: String = "",
                password2: String = "")                     

// define validation rules in companion object
// note: the field names must match to the data model
object Data {
  
  def username(username: String) = {
    if (username.isEmpty) ValidationResult.withError("Please specify username")
    else ValidationResult.success
  }

  def password1(password1: String) = // simpler syntax
    Validator(password.isEmpty, "Please specify password")
  
  def password2(password1: String, password2: Option[String]) = {  
    // you can "inject" any other fields as Options 
    if (password1.isEmpty) ValidationResult.withError("Please specify password")
    else if (password2.nonEmpty && password1 != password2.get) ValidationResult.withError("Must match password.")
    else ValidationResult.success
  }

  // define global (not tied to a specific field) validation rules here
  def $global(data: Data) = {
    ValidationResult.success
  }
  
  
  ...

  // then define the form layout fields -- probably inside the component's Backend
  
  object FormDescription extends FormBinder.FormDescription[Data] {

    // field names must match the data model's field names
    val username = FormFieldDescriptor((a: FormFieldArgs[String]) =>
      MuiTextField(
        floatingLabelText = "Username",
        onChange = (e: ReactEventI) => a.onChangeHandler(e.target.value),
        errorText = a.currentValidationResult.errorMessage)()
    )
    val password1 = ...
    val password2 = ...

    // define what to do after form data and/or form validation changes
    override def onChange(newData: Option[Data],
                          allFieldValidationResults: List[ValidationResult],
                          globalFormValidationResult: ValidationResult): Callback = {
      $.modState(_.copy(data = newData, globalValidationResult = globalFormValidationResult))
    }
  }
  
  // this binds the data model, validation rules and form description together
  val form = FormBinder.bind[Data](FormDescription)

  // use it like this:
  val handleSubmit: Callback = $.state >>= { (state: State)  =>
    if (state.globalFormValidationResult.isValid) {
      // do something with state.data.get
    } else form.showUninitializedFieldErrors
  }
  
  def render(state: State) = <.div(
    <.form(
      ^.onSubmit --> handleSubmit,
      ^.display.flex,
      ^.flexDirection.column,
      loginForm.field(LoginFormDescription.username),
      loginForm.field(LoginFormDescription.password)
    ),
    if (!state.globalValidationResult.isValid) {
      <.div(state.globalValidationResult.errorMessage)
    } else ""
  )
```


## TODO

* add prebuild FormFieldDescriptors for material-ui
