# FormBinder

FormBinder is a tool to bind
[scalajs-react](https://github.com/japgolly/scalajs-react)
form fields to a data model plus validation.

My main goal was to provide a data binding between form fields and
data model and a way to specify extra validation rules without loosing 
flexibility of the form design.

## Installation

Currently it is available as a sonatype snapshot, add this to your build.sbt:

```
libraryDependencies += "com.github.torstenrudolf.scalajs-react-form-binder" %%% "core" % "0.0.1-SNAPSHOT"
```

If you want to use the materialui form-field descriptors, you'll need this as well:
```
libraryDependencies += "com.github.torstenrudolf.scalajs-react-form-binder" %%% "extras" % "0.0.1-SNAPSHOT"
```


You also might need to add the resolver for the sonatype snapshot repo:
```
resolvers ++= Resolver.sonatypeRepo("snapshots")
```

## Usage
See demo: https://torstenrudolf.github.io/scalajs-react-form-binder

* define the data model as a `case class`
* optionally define validation rules in an object
* define the form fields in a FormLayout object
* the matching of the field-names is type-safe as it is done at compile time via a macro
* have full control over display of the form fields inside your `render` method

## Contribution

Contribution is welcome. I am fairly new to scala and this project is more of a testing ground for me.
Please file issues and/or pull requests! I am also happy to hear your opinion if anything could be done better or 
in a different way :-)

## TODO

* add tests
