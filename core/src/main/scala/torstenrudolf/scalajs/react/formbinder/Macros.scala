package torstenrudolf.scalajs.react.formbinder

import japgolly.scalajs.react.{Callback, ReactNode}


object Macros {
  // Thanks to https://github.com/Voltir/form.rx for the idea!

  def getCaseClassArgumentsDefaultValues(c: scala.reflect.macros.blackbox.Context)(tpe: c.Type): c.Expr[Map[String, Any]] = {
    // found at http://stackoverflow.com/a/21970758
    import c.universe._
    val classSym = tpe.typeSymbol
    val moduleSym = classSym.companionSymbol
    val apply = moduleSym.typeSignature.declaration(newTermName("apply")).asMethod
    // can handle only default parameters from the first parameter list
    // because subsequent parameter lists might depend on previous parameters
    val kvps = apply.paramss.head.map(_.asTerm).zipWithIndex.flatMap { case (p, i) =>
      if (!p.isParamWithDefault) None
      else {
        val getterName = newTermName("apply$default$" + (i + 1))
        Some(q"${p.name.toString} -> $moduleSym.$getterName")
      }
    }
    c.Expr[Map[String, Any]](q"Map[String, Any](..$kvps)")
  }

  def getCompanion(c: scala.reflect.macros.blackbox.Context)(tpe: c.Type): c.universe.RefTree = {
    import c.universe._
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    c.universe.internal.gen.mkAttributedRef(pre, tpe.typeSymbol.companion)
  }

  def generate[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)
                                (formLayout: c.Expr[FormLayout[T]],
                                 validatorObject: c.Expr[Any]): c.Expr[Form[T]] = {

    import c.universe._
    val targetTpe = weakTypeOf[T]
    val targetCompanion = getCompanion(c)(targetTpe)

    //Collect all fields for the target type
    val targetFields = targetTpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    if (targetFields.exists(_.name.toString == "$global")) {
      c.abort(c.enclosingPosition, s"You cannot name a field `$$global`. `$$global` is reserved methodName for the global model validator.")
    }

    val targetFieldNames = targetFields.map(_.name.toString)

    val targetFieldDefaultValues = getCaseClassArgumentsDefaultValues(c)(targetTpe)

    val formFieldDescriptors = formLayout.actualType.decls.map(_.asTerm).filter(_.isAccessor)
      .filter(_.asMethod.returnType.<:<(typeOf[FormFieldDescriptor[_]]))
      .toList

    // check that targetFields and formFieldDescriptors match
    val missing = targetFields.map(_.name.toString).toSet.diff(formFieldDescriptors.map(_.name.toString).toSet)
    if (missing.nonEmpty) {
      c.abort(c.enclosingPosition, s"The form layout is not fully defined: Missing formFieldDescriptors are:\n--- ${missing.mkString("\n--- ")}")
    }

    // check types
    val nonMatchingFieldTypes = formFieldDescriptors.toSet.diff(
      formFieldDescriptors.filter(x =>
        List(targetFields.find(_.name == x.name).get.info) == x.accessed.info.typeArgs).toSet)

    if (nonMatchingFieldTypes.nonEmpty) {
      c.abort(
        c.enclosingPosition,
        s"The formFieldDescriptor and target field types do not match. " +
          s"check fields: ${nonMatchingFieldTypes.map(f => s"${f.name.toString}: ${f.info.toString}")}")
    }

    // collect all targetFieldValidators
    val targetFieldValidators = validatorObject.actualType.decls.map(_.asTerm).filter(_.isMethod).map(_.asMethod)
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
        x.paramLists.head.head.info == targetFields.find(_.name.toString == x.name.toString).get.info ||
        x.paramLists.head.tail.forall {
          case p@q"Option[$t]" =>
            val correspondingField = targetFields.find(_.name.toString == p.name.toString)
            correspondingField.nonEmpty && t == correspondingField.get.info
          case _ => false
        }
    }) {
      c.abort(
        c.enclosingPosition,
        s"Wrong parameter list found in field validators. They need to be of the form: def maxRange(maxRange: Int, minRange: Option[Int], ..): torstenrudolf.scalajs.react.formbinder.ValidationResult ")
    }


    val targetFieldValidatorsInfo = targetFieldValidators.map(x => (x.name.toString, (x, x.paramLists.head))).toMap

    val globalTargetValidator = validatorObject.actualType.decls.map(_.asTerm).filter(_.isMethod).map(_.asMethod)
      .find(_.name.toString == "$global")

    // check for correct type
    if (!globalTargetValidator.forall(v => {
      v.returnType.<:<(typeOf[ValidationResult]) &&
        v.paramLists.size == 1 && v.paramLists.head.size == 1 &&
        v.paramLists.head.head.info == targetTpe
    })) {
      c.abort(c.enclosingPosition,
        s"Type of global validator must match the target type. (def $$global(data: ${globalTargetValidator.get.accessed.info.typeArgs}): torstenrudolf.scalajs.react.formbinder.ValidationResult = ...)")
    }

    val transformedGlobalTargetValidator =
      globalTargetValidator.map(v => q"$v")
        .getOrElse(q"(d: $targetTpe) => torstenrudolf.scalajs.react.formbinder.ValidationResult.Success")

    case class CompoundField(targetField: c.universe.Symbol,
                             defaultValueExpr: c.universe.Tree,
                             transformedTargetFieldValidator: c.universe.Tree,
                             name: String,
                             termName: c.universe.TermName,
                             formFieldDescriptor: c.universe.TermSymbol) {
      val fieldType = targetField.info
      val formFieldBindingValDef = q"val $termName: torstenrudolf.scalajs.react.formbinder.FormFieldBinding[${targetField.info}]"
    }

    val compoundFields = targetFields.zipWithIndex.map { case (f, idx) =>
      val fieldName = f.name.toString

      val validatorAndParamsOpt = targetFieldValidatorsInfo.get(fieldName)

      val paramListFieldDescriptors = validatorAndParamsOpt.toList
        .flatMap(_._2.tail.map(x => formFieldDescriptors.find(_.name.toString == x.name.toString).get))
        .map(d => q"$formLayout.$d")

      val transformedTargetFieldValidator = validatorAndParamsOpt
        .map(v =>
          q"""(fieldValue: ${v._2.head.info}, parentForm: torstenrudolf.scalajs.react.formbinder.FormAPI[_]) => {
                $validatorObject.${v._1}(
                   fieldValue,
                   ..${paramListFieldDescriptors.map(d => q"parentForm.fieldBinding($d).currentValidatedValue")})
              }
            """)
        .map(v => q"Some($v)").getOrElse(q"None")

      CompoundField(
        targetField = f,
        defaultValueExpr = q"$targetFieldDefaultValues.get($fieldName).asInstanceOf[Option[${f.info}]]",
        transformedTargetFieldValidator = transformedTargetFieldValidator,
        name = fieldName,
        termName = TermName(fieldName),
        formFieldDescriptor = formFieldDescriptors.find(_.name.toString == fieldName).get
      )
    }

    val newTree =
      q"""
      new torstenrudolf.scalajs.react.formbinder.Form[$targetTpe] with torstenrudolf.scalajs.react.formbinder.FormAPI[$targetTpe] {
        override val formLayout: torstenrudolf.scalajs.react.formbinder.FormLayout[$targetTpe] = $formLayout

        override def globalValidator(data: $targetTpe): torstenrudolf.scalajs.react.formbinder.ValidationResult =
          ($transformedGlobalTargetValidator)(data)

        override var isInitializing: Boolean = true

        private case class FieldBindingsHolder(..${compoundFields.map(_.formFieldBindingValDef)})
        private val fieldBindingsHolder = new FieldBindingsHolder(..${compoundFields.map(f => q"""torstenrudolf.scalajs.react.formbinder.FormFieldBinding[${f.fieldType}]($formLayout.${f.formFieldDescriptor}, ${f.defaultValueExpr}, ${f.transformedTargetFieldValidator}, this, ${f.name})""")})

        val allFormFieldBindings = ${compoundFields.map(f => q"fieldBindingsHolder.${f.termName}")}

        isInitializing = false
        onChangeCB().runNow()  // update default values

        override def currentUnvalidated: scala.util.Try[$targetTpe] = {
          val fieldValues = allFormFieldBindings.map(_.currentValidatedValue)
          if (fieldValues.forall(_.nonEmpty)) {
            val d = $targetCompanion.apply(..${compoundFields.map(f => q"fieldBindingsHolder.${f.termName}.currentValidatedValue.get")})
            scala.util.Success(d)
          } else {
            scala.util.Failure(torstenrudolf.scalajs.react.formbinder.FormUninitialized)
          }
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
    c.Expr[Form[T]](newTree)
  }

}


case class FormFieldBinding[O](formFieldDescriptor: FormFieldDescriptor[O],
                               defaultValue: Option[O],
                               transformedTargetFieldValidator: Option[(O, FormAPI[_]) => ValidationResult],
                               parentForm: Form[_] with FormAPI[_],
                               fieldName: String) {


  private var _currentRawValue: Option[O] = defaultValue
  private var _currentValidationResult: ValidationResult = ValidationResult.Success
  private var _showUnitializedError: Boolean = false

  def currentRawValue = _currentRawValue

  def currentValidatedValue: Option[O] = _currentValidationResult match {
    case vr if vr.isValid => _currentRawValue
    case _ => None
  }

  def currentValidationResult = _currentValidationResult

  def formField: ReactNode = {
    formFieldDescriptor.descr(FormFieldArgs[O](
      currentValue = currentValidatedValue,
      currentValidationResult = currentValidationResult,
      onChangeCB = (v: O) => updateValue(v),
      resetCB = reset,
      clearCB = clear,
      fieldName = fieldName,
      parentForm = parentForm))
  }

  def reset(): Callback = {
    //    println(s"$fieldName: resetting")
    _currentRawValue = defaultValue
    parentForm.onChangeCB()
  }

  def clear(): Callback = {
    //    println(s"$fieldName: clearing")
    _currentRawValue = None
    parentForm.onChangeCB()
  }

  def updateValue(v: O): Callback = {
    //    println(s"$fieldName: updateValue: $v")
    _currentRawValue = Some(v)
    validate()
    parentForm.onChangeCB()
  }

  def showUninitializedError: Callback = {
    _showUnitializedError = true
    validate()
    parentForm.onChangeCB()
  }

  def validate(): Unit = {
    _currentRawValue match {
      case Some(v) =>
        _currentValidationResult = transformedTargetFieldValidator.map(_.apply(v, parentForm)).getOrElse(ValidationResult.Success)
      case None if _showUnitializedError => _currentValidationResult = ValidationResult.withError("required")
      case None => _currentValidationResult = ValidationResult.Success
    }
    //    println(s"$fieldName: raw: ${_currentRawValue}, validationResult: ${_currentValidationResult}, $showUninitializedError")

  }

}

trait FormAPI[T] extends Form[T] {
  protected var isInitializing: Boolean
  val formLayout: FormLayout[T]

  def globalValidator(data: T): ValidationResult

  def validate(data: T): (Option[T], ValidationResult) = {
    globalValidator(data) match {
      case r if !r.isValid => (None, r)
      case _ => (Some(data), ValidationResult.Success)
    }
  }

  def onChangeCB(): japgolly.scalajs.react.Callback = {
    if (!isInitializing) {
      allFormFieldBindings.foreach(_.validate())
      validatedCurrentData match {
        case (dataOption, validationResult) =>
          formLayout.onChange(dataOption, allFieldValidationResults, validationResult)
      }
    } else {
      japgolly.scalajs.react.Callback.empty
    }
  }

  def currentUnvalidated: scala.util.Try[T]

  def validatedCurrentData: (Option[T], ValidationResult) = {
    currentUnvalidated match {
      case scala.util.Success(d) => validate(d)
      case scala.util.Failure(f) => (None, ValidationResult.Success)
    }
  }

  def showUninitializedFieldErrors: Callback = allFormFieldBindings.map(_.showUninitializedError).reduce {_ >> _}

  val allFormFieldBindings: List[FormFieldBinding[_]]

  def allFieldValidationResults: List[ValidationResult] = allFormFieldBindings.map(_.currentValidationResult)

  override def field[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A]): ReactNode =
    fieldBinding(fd).formField

  def fieldBinding[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A]): torstenrudolf.scalajs.react.formbinder.FormFieldBinding[A]

  override def fieldValue[A](fd: FormFieldDescriptor[A]): Option[A] = fieldBinding(fd).currentValidatedValue

  override def setFieldValue[A](fd: torstenrudolf.scalajs.react.formbinder.FormFieldDescriptor[A], value: A): Callback =
    fieldBinding(fd).updateValue(value)

}
