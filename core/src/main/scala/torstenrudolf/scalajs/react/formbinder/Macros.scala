package torstenrudolf.scalajs.react.formbinder

import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo, ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js
import scala.util.{Failure, Success, Try}


object Macros {
  // Thanks to https://github.com/Voltir/form.rx for the idea!

  def getDefaultValuesFromCompanionObject(c: scala.reflect.macros.blackbox.Context)(companion: c.Symbol): c.Expr[Map[String, Any]]  = {
    import c.universe._
    val applyMethod = companion.info.decls.find(_.name == TermName("apply")).get.asMethod
    // can handle only default parameters from the first parameter list
    // because subsequent parameter lists might depend on previous parameters
    val paramName2DefaultValue = applyMethod.paramLists.head.map(_.asTerm).zipWithIndex.flatMap { case (p, i) =>
      if (!p.isParamWithDefault) Nil
      else {
        val getterName = TermName("apply$default$" + (i + 1))
        List(q"${p.name.toString} -> $companion.$getterName")
      }
    }
    c.Expr[Map[String, Any]](q"Map[String, Any](..$paramName2DefaultValue)")
  }

  def getCompanion(c: scala.reflect.macros.blackbox.Context)(tpe: c.Type): c.universe.RefTree = {
    import c.universe._
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    c.universe.internal.gen.mkAttributedRef(pre, tpe.typeSymbol.companion)
  }

  def generateWithoutDefault[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)
                                              (formLayout: c.Expr[FormLayout[T]],
                                               validatorObject: c.Expr[Any]): c.Expr[Form[T]] = {
    generateFromExplicitValues[T](c)(formLayout, validatorObject, c.universe.reify(None))
      .asInstanceOf[c.Expr[Form[T]]]  // unnecessary, but idea showed error otherwise
  }

  def generateWithDefault[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)
                                           (formLayout: c.Expr[FormLayout[T]],
                                            validatorObject: c.Expr[Any],
                                            defaultModelValue: c.Expr[T]): c.Expr[Form[T]] = {
    import c.universe._
    generateFromExplicitValues[T](c)(formLayout, validatorObject, reify[Option[T]](Some(defaultModelValue.splice)))
      .asInstanceOf[c.Expr[Form[T]]]  // unnecessary, but idea showed error otherwise
  }

  def generateOnFormBinder[DataModel: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context): c.Expr[Form[DataModel]] = {
    import c.universe._

    val dataModelFields = c.prefix.actualType.members
      .find(m => m.isClass && m.name == TypeName("DataModel")).get.info.members
      .collect{case v: TermSymbol => v.asTerm}

    val fields = c.prefix.actualType.members
      .find(m => m.isClass && m.name == TypeName("DataModel")).get.info.decls
      .collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.get.paramLists.head

    val defaultModelValue = c.prefix.actualType.members.find(m => m.name == TermName("defaultFormValue")).get

    val dataModelType = c.prefix.actualType.members.find(m => m.isClass && m.name == TypeName("DataModel")).get.asType
    val formLayout = c.prefix.actualType.members.find(m => m.isTerm && m.name == TermName("formLayout")).get.asTerm
    val validatorsHolder = c.prefix.actualType.members.find(m => m.isTerm && m.name == TermName("validatorsHolder")).get.asTerm
    val defaultFormValue = c.prefix.actualType.members.find(m => m.isTerm && m.name == TermName("defaultFormValue")).get.asTerm

    c.Expr[Form[DataModel]](formImpl(c)(dataModelType, formLayout, validatorsHolder, c.Expr[Option[_]](q"$defaultFormValue")))

  }

  def generateFromExplicitValues[DataModel: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)
                                                          (formLayout: c.Expr[FormLayout[DataModel]],
                                                           validatorsHolder: c.Expr[Any],
                                                           defaultFormValue: c.Expr[Option[DataModel]]): c.Expr[Form[DataModel]] = {
    import c.universe._
    val dataModelTpe = weakTypeOf[DataModel]
    val dataModelCompanion =  getCompanion(c)(dataModelTpe).symbol // had problems when using abstract classes
    c.Expr[Form[DataModel]](formImpl(c)(
      dataModelTpe.typeSymbol,
      formLayout.tree.symbol,
      validatorsHolder.tree.symbol,
      defaultFormValue))
  }

  def formImpl(c: scala.reflect.macros.blackbox.Context)
                (dataModelTypeSymbol: c.universe.Symbol,
                 formLayout: c.universe.Symbol,
                 validatorsHolder: c.universe.Symbol,
                 defaultFormValue: c.Expr[Option[_]]): c.universe.Tree =
  {

    import c.universe._

    val dataModelCompanion = dataModelTypeSymbol.companion

//    val dataModelCompanion =  getCompanion(c)(dataModelTypeSymbol.info) // had problems when using abstract classes


    //Collect all fields for the target type
    //    val targetFields = dataModelCompanion.actualType.decls.find(_.name == TermName("apply")).get.asMethod.paramLists.head
    val dataModelFields = dataModelTypeSymbol.info.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    if (dataModelFields.exists(_.name.toString == "$global")) {
      c.abort(c.enclosingPosition, s"You cannot name a field `$$global`. `$$global` is reserved methodName for the global model validator.")
    }

    val targetFieldNames = dataModelFields.map(_.name.toString)

    val targetFieldDefaultValues: c.Expr[Map[String, Any]] =
      c.Expr[Map[String, Any]](
        q"""
           $defaultFormValue.map((dmv: $dataModelTypeSymbol)=> Map[String, Any](..${dataModelFields.map(fn => (fn.asTerm.name.toString, q"dmv.${fn.asTerm.name}"))})).getOrElse(${getDefaultValuesFromCompanionObject(c)(dataModelCompanion)})
          """)

    val preSortedFFDs = formLayout.info.members.sorted
    val formFieldDescriptors = formLayout.info.members
      .sorted  // sorts in order of declaration, but inherited members appear later
      .sortBy(ffd => {
        //resort by order of fields in DataModel, but keep decls of read-only-fields where they were declared
        val i = targetFieldNames.indexOf(ffd.name.toString)
        if (i != -1) i
        else preSortedFFDs.indexOf(ffd) - 0.5  // keep it where it was
      })
      .map(_.asTerm)
      .filter(_.isAccessor)
      .filter(_.asMethod.returnType.<:<(typeOf[FormFieldDescriptor[_]]))

    // check that targetFields and formFieldDescriptors match
    val missing = dataModelFields.map(_.name.toString).toSet.diff(formFieldDescriptors.map(_.name.toString).toSet)
    if (missing.nonEmpty) {
      c.abort(c.enclosingPosition, s"The form layout is not fully defined: Missing formFieldDescriptors are:\n--- ${missing.mkString("\n--- ")}")
    }

    // check types
    val nonMatchingFieldTypes = formFieldDescriptors.toSet.diff(
      formFieldDescriptors.filter(ffd =>
        ffd.info.resultType.typeArgs.size == 1 && {
          dataModelFields.find(_.name == ffd.name) match {
            case Some(targetField) => targetField.info == ffd.info.resultType.typeArgs.head
            case None => ffd.info.resultType.typeArgs.head == typeOf[Unit]  // is calculated field
          }
        }
      ).toSet
    )

    if (nonMatchingFieldTypes.nonEmpty) {
      c.abort(
        c.enclosingPosition,
        s"The formFieldDescriptor and target field types do not match. " +
          s"check fields: ${nonMatchingFieldTypes.map(f => s"${f.name.toString}: ${f.info.toString}")}")
    }

    // collect all targetFieldValidators
    val targetFieldValidators = validatorsHolder.info.members.map(_.asTerm).filter(_.isMethod).map(_.asMethod)
      .filter(v => targetFieldNames.contains(v.name.toString))

    val targetFieldValidatorsWrongType =
      targetFieldValidators.filter { x => !x.returnType.<:<(typeOf[ValidationResult]) }
    if (targetFieldValidatorsWrongType.nonEmpty) {
      c.abort(
        c.enclosingPosition,
        s"Field validators do not all return torstenrudolf.scalajs.react.formbinder.ValidationResult check ${targetFieldValidatorsWrongType.map(_.name.toString)}")
    }
    if (!targetFieldValidators.forall { x =>
      x.paramLists.size == 1 ||
        x.paramLists.head.head.name == x.name ||
        x.paramLists.head.head.info == dataModelFields.find(_.name.toString == x.name.toString).get.info ||
        x.paramLists.head.tail.forall {
          case p@q"Option[$t]" =>
            val correspondingField = dataModelFields.find(_.name.toString == p.name.toString)
            correspondingField.nonEmpty && t == correspondingField.get.info
          case _ => false
        }
    }) {
      c.abort(
        c.enclosingPosition,
        s"Wrong parameter list found in field validators. They need to be of the form: def maxRange(maxRange: Int, minRange: Option[Int], ..): torstenrudolf.scalajs.react.formbinder.ValidationResult ")
    }


    val targetFieldValidatorsInfo = targetFieldValidators.map(x => (x.name.toString, (x, x.paramLists.head))).toMap

    val globalTargetValidator = validatorsHolder.info.members.map(_.asTerm).filter(_.isMethod).map(_.asMethod)
      .find(_.name.toString == "$global")

    // check for correct type
    if (!globalTargetValidator.forall(v => {
      v.returnType.<:<(typeOf[ValidationResult]) &&
        v.paramLists.size == 1 && v.paramLists.head.size == 1 &&
        v.paramLists.head.head.info.typeSymbol == dataModelTypeSymbol
    })) {
      c.abort(c.enclosingPosition,
        s"Type of global validator must match the target type. (def $$global(data: ${globalTargetValidator.get.accessed.info.typeArgs}): torstenrudolf.scalajs.react.formbinder.ValidationResult = ...)")
    }

    val transformedGlobalTargetValidator =
      globalTargetValidator.map(v => q"$v")
        .getOrElse(q"(d: $dataModelTypeSymbol) => torstenrudolf.scalajs.react.formbinder.ValidationResult.Success")

    case class CompoundField(targetField: Option[c.universe.Symbol],
                             transformedTargetFieldValidator: c.universe.Tree,
                             name: String,
                             termName: c.universe.TermName,
                             formFieldDescriptor: c.universe.TermSymbol) {
      val hasTargetField = targetField.isDefined
      val fieldType = targetField.map(_.info).getOrElse(typeOf[Unit])
      val isString = fieldType =:= typeOf[String]
      val formFieldBindingValDef = q"val $termName: torstenrudolf.scalajs.react.formbinder.FormFieldBinding[$fieldType]"
      val defaultValueExpr = q"$targetFieldDefaultValues.get($name).asInstanceOf[Option[${fieldType}]]"
    }

    val compoundFields = formFieldDescriptors.zipWithIndex.map{ case (ffd, idx) =>
      val fieldName = ffd.name.toString
      val dataModelField = dataModelFields.find(_.name.toString == ffd.name.toString)

      val validatorAndParamsOpt = targetFieldValidatorsInfo.get(fieldName)

      val paramListFieldDescriptors = validatorAndParamsOpt.toList
        .flatMap(_._2.tail.map(x => formFieldDescriptors.find(_.name.toString == x.name.toString).get))
        .map(d => q"$formLayout.$d")

      val transformedTargetFieldValidator = validatorAndParamsOpt
        .map(v =>
          q"""(fieldValue: ${v._2.head.info}, parentForm: torstenrudolf.scalajs.react.formbinder.FormAPI[_]) => {
                $validatorsHolder.${v._1}(
                   fieldValue,
                   ..${paramListFieldDescriptors.map(d => q"parentForm.fieldBinding($d).currentValidatedValue")})
              }
            """)
        .map(v => q"Some($v)").getOrElse(q"None")

      CompoundField(
        targetField = dataModelField,
        transformedTargetFieldValidator = transformedTargetFieldValidator,
        name = fieldName,
        termName = TermName(fieldName),
        formFieldDescriptor = ffd
      )
    }


    val newTree =
      q"""
      new torstenrudolf.scalajs.react.formbinder.Form[$dataModelTypeSymbol] with torstenrudolf.scalajs.react.formbinder.FormAPI[$dataModelTypeSymbol] {
        override val formLayout: torstenrudolf.scalajs.react.formbinder.FormLayout[$dataModelTypeSymbol] = $formLayout

        override def globalValidator(data: $dataModelTypeSymbol): torstenrudolf.scalajs.react.formbinder.ValidationResult =
          ($transformedGlobalTargetValidator)(data)

        override var isInitializing: Boolean = true

        private case class FieldBindingsHolder(..${compoundFields.map(_.formFieldBindingValDef)})
        private val fieldBindingsHolder = new FieldBindingsHolder(..${compoundFields.map(f => q"""torstenrudolf.scalajs.react.formbinder.FormFieldBinding[${f.fieldType}]($formLayout.${f.formFieldDescriptor}, ${f.defaultValueExpr}, ${f.isString}, ${f.transformedTargetFieldValidator}, this, ${f.name}, ${f.hasTargetField})""")})

        val allFormFieldBindings: List[torstenrudolf.scalajs.react.formbinder.FormFieldBinding[_]] = ${compoundFields.map(f => q"fieldBindingsHolder.${f.termName}")}

        isInitializing = false
        onChangeCB.runNow()  // update default values

        override def currentValueWithoutGlobalValidation: scala.util.Try[$dataModelTypeSymbol] = {
          val fieldValues = allFormFieldBindingsWithUnderlyingDataField.map(_.currentValidatedValue)
          if (fieldValues.forall(_.nonEmpty)) {
            val d = $dataModelCompanion.apply(..${compoundFields.filter(_.hasTargetField).map(f => q"${f.termName} = fieldBindingsHolder.${f.termName}.currentValidatedValue.get")})
            scala.util.Success(d)
          } else {
            scala.util.Failure(torstenrudolf.scalajs.react.formbinder.FormUninitialized)
          }
        }

        override def setModelValue(newModelValue: $dataModelTypeSymbol): japgolly.scalajs.react.Callback = {
        ${compoundFields.filter(_.hasTargetField).map(f =>
        q"""fieldBindingsHolder.${f.termName}.updateValue(newModelValue.${f.termName}) match {
              case scala.util.Success(cb) => cb
              case scala.util.Failure(e) => throw torstenrudolf.scalajs.react.formbinder.FormUninitialized
            }""")}.foldLeft(japgolly.scalajs.react.Callback.empty)(_ >> _)
        }

        override def allFields: List[japgolly.scalajs.react.ReactNode] = {
          ${compoundFields.map(f => q"fieldBindingsHolder.${f.termName}.formField")}
        }

        override def fieldBinding[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A]): torstenrudolf.scalajs.react.formbinder.FormFieldBinding[A] = {
          ${compoundFields.map(f => q"fieldBindingsHolder.${f.termName}")}.find(fb => fb.formFieldDescriptor == fd) match {
            case Some(fb) => fb.asInstanceOf[torstenrudolf.scalajs.react.formbinder.FormFieldBinding[A]]
            case _ => throw new RuntimeException("This FormFieldDescriptor does not belong to this form.")
          }
        }

      }
    """
    //    println(show(newTree))
    newTree
  }
}

object FormField {

  case class Props[O](formFieldDescriptor: FormFieldDescriptor[O],
                      onChangeCB: (Option[O]) => Callback,
                      defaultValue: Option[O],
                      fieldName: String,
                      valueIsString: Boolean,
                      validator: (O) => ValidationResult,
                      parentForm: Form[_] with FormAPI[_])

  case class State[O](currentValidationResult: Option[ValidationResult] = None,
                      currentRawValue: Option[O] = None,
                      showUnitializedError: Boolean = false)

  class Backend[O]($: BackendScope[Props[O], State[O]]) {

    def validate(showUnitializedError: Boolean = false): CallbackTo[ValidationResult] = $.state.zip($.props) >>= { case (state, props) =>
      val showUnitializedErrorX = showUnitializedError || state.showUnitializedError
      val valueToValidate =
        if (showUnitializedErrorX && state.currentRawValue.isEmpty && props.valueIsString) {
          // todo: introduce a generic uninitializedValue for all value types
          Some("".asInstanceOf[O])
        }
        else state.currentRawValue

      val validationResult = valueToValidate match {
        case Some(v) => props.validator(v)
        case None if showUnitializedErrorX => ValidationResult.withError("required") // todo: make required error string configurable
        case None => ValidationResult.Success
      }

      $.modState(_.copy(currentValidationResult = Some(validationResult), showUnitializedError = showUnitializedErrorX)) >> CallbackTo(validationResult)
    }

    def forceUpdate: Callback = $.forceUpdate

    private def onChangeCB: Callback =
      $.state.zip($.props) >>= { case (state, props) => props.onChangeCB(currentValidatedValue) }

    def updateRawValue(v: Option[O]): Callback = {
      $.modState(_.copy(currentRawValue = v), cb = validate() >> onChangeCB)
    }

    def clear: Callback = $.modState(_.copy(showUnitializedError = false), cb = updateRawValue(None))

    def resetToDefault: Callback = $.modState(_.copy(showUnitializedError = false), cb = {$.props >>= { props => updateRawValue(props.defaultValue) }})

    def currentValidatedValue: Option[O] = {
      if (currentValidationResult.isValid) $.state.runNow().currentRawValue else None
    }

    def currentValidationResult: ValidationResult = $.state.runNow().currentValidationResult match {
      case None => validate().runNow()
      case Some(vr) => vr
    }

    def render(props: Props[O], state: State[O]) = <.div(
      props.formFieldDescriptor.descr(
        FormFieldArgs[O](
          currentValue = state.currentRawValue,
          currentValidationResult = currentValidationResult,
          onChangeCB = (v: O) => updateRawValue(Some(v)),
          resetToDefaultCB = resetToDefault,
          clearCB = clear,
          fieldName = props.fieldName,
          parentForm = props.parentForm)
      )
    )
  }

}

case class FormField[O](parentBinding: FormFieldBinding[O]) {

  val component = ReactComponentB[FormField.Props[O]]("FormField")
    .initialState(FormField.State[O]())
    .renderBackend[FormField.Backend[O]]
    .componentDidMount(scope =>
      Callback {parentBinding.setFormFieldBackend(scope.backend)} >>
        scope.modState(_.copy(currentRawValue = scope.props.defaultValue))
    )
    .build

  def apply(formFieldDescriptor: FormFieldDescriptor[O],
            onChangeCB: (Option[O]) => Callback,
            defaultValue: Option[O],
            fieldName: String,
            valueIsString: Boolean,
            validator: (O) => ValidationResult,
            parentForm: Form[_] with FormAPI[_],
            key: js.UndefOr[js.Any] = js.undefined,
            ref: js.UndefOr[String] = js.undefined) = {
    component.set(key, ref)(FormField.Props[O](formFieldDescriptor, onChangeCB, defaultValue, fieldName, valueIsString, validator, parentForm))
  }
}

case class FormFieldBinding[O](formFieldDescriptor: FormFieldDescriptor[O],
                               private val defaultValue: Option[O],
                               private val isString: Boolean,
                               transformedTargetFieldValidator: Option[(O, FormAPI[_]) => ValidationResult],
                               private val parentForm: Form[_] with FormAPI[_],
                               fieldName: String,
                               hasTargetField: Boolean) {

  def currentValidatedValue: Option[O] = formFieldBackend.flatMap(_.currentValidatedValue)

  def currentValidationResult: Try[ValidationResult] = Try(formFieldBackend.get.currentValidationResult)

  def validate(showUninitializedError: Boolean): Try[Callback] = Try(formFieldBackend.get.validate(showUninitializedError).void)

  def forceUpdateComponent: Callback = formFieldBackend.map(_.forceUpdate).getOrElse(Callback.empty)

  def updateValue(v: O): Try[Callback] = Try(formFieldBackend.get.updateRawValue(Some(v)))

  def clear: Try[Callback] = Try(formFieldBackend.get.clear)

  def resetToDefault: Try[Callback] = Try(formFieldBackend.get.resetToDefault)

  private val formFieldComp = FormField[O](this)

  private var formFieldBackend: Option[FormField.Backend[O]] = None

  def setFormFieldBackend(backend: FormField.Backend[O]) = {
    formFieldBackend = Some(backend)
  }

  def formField: ReactNode = formFieldComp(
    formFieldDescriptor = formFieldDescriptor,
    onChangeCB = (v: Option[O]) => parentForm.onChangeCB,
    defaultValue = defaultValue,
    fieldName = fieldName,
    valueIsString = isString,
    validator = transformedTargetFieldValidator.map(v => (x: O) => v(x, parentForm)).getOrElse((_: O) => ValidationResult.Success),
    parentForm = parentForm)

}


trait FormAPI[T] extends Form[T] {
  protected var isInitializing: Boolean
  protected val formLayout: FormLayout[T]

  private var _validatedFormData: Option[T] = None
  private var _formGlobalValidationResult: Option[ValidationResult] = None
  override def globalValidationResult: Option[ValidationResult] = _formGlobalValidationResult
  private val _formOnChangeSubscribers: scala.collection.mutable.ListBuffer[Callback] =
    scala.collection.mutable.ListBuffer.empty[Callback]

  private def formGlobalValidationResult: ValidationResult = _formGlobalValidationResult match {
    case Some(vr) => vr
    case None => throw FormUninitialized
  }

  protected def globalValidator(data: T): ValidationResult

  protected def currentValueWithoutGlobalValidation: scala.util.Try[T]

  private def validate(showUninitializedError: Boolean): Callback = {
    validateAllFields(showUninitializedError = showUninitializedError) match {
      case Success(_) => currentValueWithoutGlobalValidation match {
        case Success(data) => globalValidator(data) match {
          case r if !r.isValid =>
            _validatedFormData = None
            _formGlobalValidationResult = Some(r)
          case _ =>
            _validatedFormData = Some(data)
            _formGlobalValidationResult = Some(ValidationResult.Success)
        }
        case Failure(e) =>
          _validatedFormData = None
          _formGlobalValidationResult = None
      }
      case Failure(_) =>
        _validatedFormData = None
        _formGlobalValidationResult = None
    }

    formLayout.onChange(validatedData = _validatedFormData) >>
      _formOnChangeSubscribers.foldLeft(Callback.empty)(_ >> _)

  }

  private def validateAllFields(showUninitializedError: Boolean): Try[Unit] =
  // this validates the fields as well as triggers the field components to refresh
    Try(
      {
        allFormFieldBindingsWithUnderlyingDataField
          .map(_.validate(showUninitializedError = showUninitializedError).get) ++
          allFormFieldBindings.filter(f => !f.hasTargetField).map(_.forceUpdateComponent)
      }
        .reduce {_ >> _}
        .runNow()
    )

  override def fullValidate: Callback = validate(showUninitializedError = true)

  def onChangeCB: Callback = validate(showUninitializedError = false)

  override def validatedFormData: Option[T] = {
    validate(showUninitializedError = true).runNow()
    _validatedFormData
  }

  override def validatedFormDataNew(reValidate: Boolean): Option[T] = {
    if (reValidate) validatedFormData
    else _validatedFormData
  }

  private def allFieldValidationResults: List[ValidationResult] = allFormFieldBindingsWithUnderlyingDataField.map(
    _.currentValidationResult match {
      case Success(vr) => vr
      case _ => throw FormUninitialized
  })

  protected def allFormFieldBindings: List[FormFieldBinding[_]]

  protected def allFormFieldBindingsWithUnderlyingDataField: List[FormFieldBinding[_]] =
    allFormFieldBindings.filter(_.hasTargetField)

  override def field[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A]): ReactNode =
    fieldBinding(fd).formField

  def fieldBinding[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A]): torstenrudolf.scalajs.react.formbinder.FormFieldBinding[A]

  override def fieldValue[A](fd: FormFieldDescriptor[A]): Option[A] = fieldBinding(fd).currentValidatedValue

  override def clearField[A](fd: FormFieldDescriptor[A]): Callback = fieldBinding(fd).clear match {
    case Success(cb) => cb
    case Failure(e) => throw FormUninitialized
  }

  override def clearAllFields: Callback = {
    // note: this calls FormAPI.onChangeCB and therefore FormAPI.validate N times (N = number of form fields)
    //  this could become slow for big forms and we might need to improve this
    Try(allFormFieldBindingsWithUnderlyingDataField.map(_.clear.get).reduce {_ >> _}) match {
      case Success(cb) => cb
      case Failure(e) => throw FormUninitialized
    }
  }

  override def resetFieldToDefault[A](fd: FormFieldDescriptor[A]): Callback = fieldBinding(fd).resetToDefault match {
    case Success(cb) => cb
    case Failure(e) => throw FormUninitialized
  }

  override def resetAllFields: Callback =
    Try(allFormFieldBindingsWithUnderlyingDataField.map(_.resetToDefault.get).reduce {_ >> _}) match {
      // note: this calls FormAPI.onChangeCB and therefore FormAPI.validate N times (N = number of form fields)
      //  this could become slow for big forms and we might need to improve this
      case Success(cb) => cb
      case Failure(e) => throw FormUninitialized
    }

  override def setFieldValue[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A], value: A): Callback =
    fieldBinding(fd).updateValue(value) match {
      case Success(cb) => cb
      case Failure(e) => throw FormUninitialized
    }

  override def subscribeToUpdates(cb: Callback): Unit = _formOnChangeSubscribers.append(cb)

}
