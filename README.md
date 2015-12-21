# Binding.scala

**Binding.scala** is a data-binding framework for [Scala](http://www.scala-lang.org/) JVM and [Scala.js](http://www.scala-js.org/).

Binding.scala can be used as a **[reactive](https://en.wikipedia.org/wiki/Reactive_programming) web framework**.
It enables you use native XML literal syntax to create reactive DOM nodes,
which are able to automatically change whenever the data source changes.

## Comparison to other reactive web frameworks

Binding.scala has more features and less concepts than other reactive web frameworks like [Widok](https://widok.github.io/) or [ReactJS](https://facebook.github.io/react/).

<table>
  <thead>
    <tr>
      <th></th>
      <th>Binding.scala</th>
      <th>Widok</th>
      <th>ReactJS</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Support HTML literal?</th>
      <td>Yes</td>
      <td>No</td>
      <td>Yes</td>
    </tr>
    <tr>
      <th>Algorithm to update DOM</th>
      <td>Precise data-binding</td>
      <td>Precise data-binding</td>
      <td>Vitual DOM differentiation, which requires manually managed key attributes for complicated DOM.</td>
    </tr>
    <tr>
      <th>Lifecycle management for data-binding expressions</th>
      <td>Automatically</td>
      <td>Manually</td>
      <td>Manually</td>
    </tr>
    <tr>
      <th>Statically type checking</th>
      <td>Yes, even for HTML tags and attribues</td>
      <td>Yes</td>
      <td>No</td>
    </tr>
    <tr>
      <th>Learning curve</th>
      <td>Always easy</td>
      <td>Unfamiliar DOM creation syntax for newbie. Requires much efforts to understand its corner cases.</td>
      <td>Easy to start. Requires much more efforts to understand its corner cases.</td>
    </tr>
  </tbody>
</table>

See [Desigin](#Design) section for more information.

## Getting started

We will build an Binding.scala web page during the following steps.

### Step 0: Setup a Sbt Scala.js project

See http://www.scala-js.org/tutorial/basic/ for information about how to setup such a project.

Note that macros used by Binding.scala require a larger stack size for JVM that runs Scala compiler than default value,
therefore, you should specify a `-Xss5m` JVM argument when launching sbt:

```
sbt -J-Xss5m
```

Otherwise you may see an error message like this:

```
[trace] Stack trace suppressed: run last dom/compile:compileIncremental for the full output.
[error] (dom/compile:compileIncremental) java.lang.StackOverflowError
```

### Step 1: Add Binding.scala dependencies into your `build.sbt`:

``` scala
libraryDependencies += "au.com.realcommercial.binding" %%% "dom" % "1.0.3"
```

### Step 2: Create a `data` field, which contains some `Var` and `Vars` as data source for your data-binding expressions

``` scala
case class Contact(name: Var[String], email: Var[String])

val data = Vars.empty[Contact]
```

### Step 3: Create a `@dom` method that contains data-binding expressions

``` scala
@dom
def table = {
  <table border="1" cellPadding="5">
    <thead>
      <tr>
        <th>Name</th>
        <th>E-mail</th>
      </tr>
    </thead>
    <tbody>
      {
        for (contact <- data) yield {
          <tr>
            <td>
              {contact.name.each}
            </td>
            <td>
              {contact.email.each}
            </td>
          </tr>
        }
      }
    </tbody>
  </table>
}
```

### Step 4: render the data-binding expressions to DOM in the `main` method

``` scala
@JsExport
def main(): Unit = {
  dom.render(document.body, table)
}
```

### Step 5: Invoke the `main` method in a HTML page

``` html
<!DOCTYPE html>
<html>
  <head>
    <script type="text/javascript" src="js-fastopt.js"></script>
  </head>
  <body>
    <script type="text/javascript">
      SampleMain().main()
    </script>
  </body>
</html>
```

Now you will see a table that just contains a header. Because `data` is empty at the moment.

### Step 6: Add some `<button>` to fill `data` for the table

``` scala
@dom
def table = {
  <div>
    <button
      onclick={ event: Event =>
        data.get += Contact(Var("Yang Bo"), Var("yang.bo@rea-group.com"))
      }
    >
      Add a contact
    </button>
  </div>
  <table border="1" cellPadding="5">
    <thead>
      <tr>
        <th>Name</th>
        <th>E-mail</th>
        <th>Operation</th>
      </tr>
    </thead>
    <tbody>
      {
        for (contact <- data) yield {
          <tr>
            <td>
              {contact.name.each}
            </td>
            <td>
              {contact.email.each}
            </td>
            <td>
              <button
                onclick={ event: Event =>
                  contact.name := "Modified Name"
                }
              >
                Modify the name
              </button>
            </td>
          </tr>
        }
      }
    </tbody>
  </table>
}
```

See https://git.realestate.com.au/yang-bo/Binding.scala-sample for the complete example.


## Design

### Precise data-binding

ReactJS requires users provide a `render` function for each component.
The `render` function should map `props` and `state` to a ReactJS's virtual DOM,
then ReactJS framework creates a DOM with the same structure as the virtual DOM.

When `state` changes, ReactJS framework invokes `render` function to get a new virtual DOM.
Unfortunately, ReactJS framework does not precisely know what the `state` changing is.
ReactJS framework has to compare the new virtual DOM and the original virtual DOM,
and guesses the change set between the two virtual DOM,
then apply guessed the change set to the real DOM as well.

For example, after you prepended a table row, say, `<tr>`, into an existing `<table>`,
ReactJS may think you changed the content of the every existing `<tr>`s of the `<table>`,
and appended another `<tr>` at the tail of the `<table>`.

The reason is that the `render` function for ReactJS does not describe the relationship between `state` and DOM.
Instead, it describes the process to create a virtual DOM.
As a result, the `render` function does not provide any information about the purpose of the `state` changing
that a data-binding framework wants.

Unlike ReactJS, a Binding.scala `@dom` method is NOT a regular function.
It is a template that describes the relationship between data source and the DOM.
When partial of the data source changed, Binding.scala framework knows exactly the correspond DOM to the partial data.
Then, Binding.scala only re-evaluates partial of the `@dom` method to update the partial of DOM.

With the help of the ability of precise data-binding in Binding.scala,
you can get rid of trivial concepts,
like `key` attribute, `shouldComponentUpdate` method, `componentDidUpdate` method or `componentWillUpdate` method forced by ReactJS.

### Composability

The smallest composable unit in ReactJS is a component.
It is fair to say that a React component is lighter than an AngularJS controller,
while Binding.scala is better than that.

The smallest composable unit in Binding.scala is a `@dom` method.
Every `@dom` method is able to compose other `@dom` methods via `.each`.

``` scala
case class Contact(name: Var[String], email: Var[String])

@dom
def bindingButton(contact: Contact): Binding[Button] = {
  <button
    onclick={ event: Event =>
      contact.name := "Modified Name"
    }
  >
   Modify the name
  </button>
}

@dom
def bindingTr(contact: Contact): Binding[TableRow] = {
  <tr>
    <td>{ contact.name.each }</td>
    <td>{ contact.email.each }</td>
    <td>{ bindingButton(contact).each }</td>
  </tr>
}

@dom
def bindingTable(contacts: BindingSeq[Contact): Binding[Table] = {
  <table>
    {
      for (contact <- contacts) yield {
        bindingTr(contact).each
      }
    }
  </table>
}

@JSExport
def main(): Unit = {
  val data = Vars(Contact(Var("Yang Bo"), Var("yang.bo@rea-group.com")))
  dom.render(document.body, bindingTable(data)
}
```

You may find out this approach is much simpler than ReactJS, as:

 * Instead of passing `props` in ReactJS, you just simply provide parameters for  Binding.scala.
 * Instead of specifying `propTypes` in ReactJS, you just simply define the types parameters in Binding.scala.
 * Instead of raising a run-time error when types of props do not match in ReactJS, you just check the types at compile-time.

### Lifecycle management for data-binding expressions

The ability of precise data-binding in Binding.scala requires listener registrations on the data source.
Others reactive frameworks that has the ability ask users manage the lifecycle of data-binding expressions.

For example, [MetaRx](https://github.com/MetaStack-pl/MetaRx/issues/45) provide a `dispose` method to unregister the listeners created when building data-binding expressions.
The user of MetaRx have the responsibility to call `dispose` method for every `map` and `flatMap` call after the expression changes,
otherwise MetaRx leaks memory. Unfortunately manually `dispose` everything is almost impossible for complicated binding expression.

Another reactive web framework [Widok](https://github.com/widok/widok/issues/29) did not provide any mechanism to manage lifecycle of of data-binding expressions.
As a result, it simply always leaks memory.

Because ReactJS does not have the ability of precise data-binding,
it does not register listeners to data source hence no memory leaks issue for simple cases.

Unfortunately, ReactJS provides `ref` attribute, `getInitialState` method, `componentWillMount` method, and `componentDidMount` method,
encouraging users create operations with side-effects in these methods, which are usually error-prone.

Unlike MetaRx or Widok, all data-binding expressions are pure functional, with no side-effects in Binding.scala.
Binding.scala does not register any listeners when users create individual expressions,
thus users neither need to manually unregister listeners for a single expression like MetaRx,
nor perform additional operations in handlers like ReactJS.

Instead, Binding.scala create all internal listeners together,
when the user calls `dom.render` or `Binding.watch` on root of expressions.
`dom.render` or `Binding.watch` manages listeners in all sub-expressions together.

In brief, Binding.scala separates functionality in two kinds:
 * `@dom` methods that produce pure functional expressions with no side-effects.
 * `dom.render` or `Binding.watch` manages all side-effects automatically.

### HTML literal and statically type checking


TODO

## Downloads

Binding.scala has an extremely tiny code base. There is only one source file [Binding.scala](https://git.realestate.com.au/rca/Binding.scala/blob/master/core/src/main/scala/au/com/realcommercial/binding/Binding.scala) for data-binding expressions,
and one source file [dom.scala](https://git.realestate.com.au/rca/Binding.scala/blob/master/dom/src/main/scala/au/com/realcommercial/binding/dom.scala) for HTML DOM integration.

### Core data-binding expressions (Binding.scala)

The former module is available for both JVM and Scala.js. You could add it in your `build.sbt`.

``` scala
// For JVM projects
libraryDependencies += "au.com.realcommercial.binding" %% "core" % "1.0.3"
```

``` scala
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "au.com.realcommercial.binding" %%% "core" % "1.0.3"
```
You could download the API documentation of core module at Nexus:

 * [core_2.11-1.0.3-javadoc.jar](http://nexus.delivery.realestate.com.au/nexus/content/repositories/releases/au/com/realcommercial/binding/core_2.11/1.0.3/core_2.11-1.0.3-javadoc.jar)

### HTML DOM integration (dom.scala)

The latter module is only available for Scala.js. You could add it in your `build.sbt`.

``` scala
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "au.com.realcommercial.binding" %%% "dom" % "1.0.3"
```

You could download the API documentation of dom module at Nexus:

 * [dom_2.11-1.0.3-javadoc.jar](http://nexus.delivery.realestate.com.au/nexus/content/repositories/releases/au/com/realcommercial/binding/dom_2.11/1.0.3/dom_2.11-1.0.3-javadoc.jar)
