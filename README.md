# Binding.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Production Ready](https://img.shields.io/badge/%F0%9F%91%8C-Production%20Ready-00ddcc.svg)](https://github.com/search?l=Scala&o=desc&q="com.thoughtworks.binding"&s=indexed&type=Code&utf8=✓)
[![Extremely Lightweight](https://img.shields.io/badge/%F0%9F%A6%8B-Extremely%20Lightweight-7799cc.svg)](http://todomvc.com/examples/binding-scala/)

[![Join the chat at https://gitter.im/ThoughtWorksInc/Binding.scala](https://badges.gitter.im/ThoughtWorksInc/Binding.scala.svg)](https://gitter.im/ThoughtWorksInc/Binding.scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![StackOverflow](https://img.shields.io/stackexchange/stackoverflow/t/binding.scala.svg?label=StackOverflow+questions)](https://stackoverflow.com/questions/tagged/binding.scala?sort=votes)
[![Scala CI](https://github.com/ThoughtWorksInc/Binding.scala/actions/workflows/scala.yml/badge.svg)](https://github.com/ThoughtWorksInc/Binding.scala/actions/workflows/scala.yml)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.binding/binding_2.13.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.binding/binding_2.13/latest/com/thoughtworks/binding/index.html)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/Binding.scala/latest.svg)](https://index.scala-lang.org/thoughtworksinc/binding.scala)

**Binding.scala** is a data-binding library for [Scala](http://www.scala-lang.org/), running on both JVM and [Scala.js](http://www.scala-js.org/).

Binding.scala can be used as the basis of UI frameworks, however latest Binding.scala 12.x does not contain any build-in UI frameworks any more. For creating reactive HTML UI, you may want to check out [html.scala](https://github.com/Atry/html.scala), which is an UI framework based on Binding.scala, and it is also the successor of previously built-in [dom](https://javadoc.io/page/com.thoughtworks.binding/dom_sjs0.6_2.12/latest/com/thoughtworks/binding/dom.html) library. See also [React / Binding.scala / html.scala Interoperability](https://github.com/Atry/ReactToBindingHtml.scala) for using existing React components with Binding.scala, 

See [Binding.scala • TodoMVC](http://todomvc.com/examples/binding-scala/) or [ScalaFiddle DEMOs](https://github.com/ThoughtWorksInc/Binding.scala/wiki/ScalaFiddle-DEMOs) as examples for common tasks when working with Binding.scala.

## Comparison to other reactive web frameworks

Binding.scala and html.scala has more features and less concepts than other reactive web frameworks like [ReactJS](https://facebook.github.io/react/).

<table>
  <thead>
    <tr>
      <th></th>
      <th>Binding.scala</th>
      <th>ReactJS</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>Support HTML literal?</th>
      <td>Yes</td>
      <td>Partially supported. Regular HTML does not compile, unless developers manually replaces <code>class</code> and <code>for</code> attributes to <code>className</code> and <code>htmlFor</code>, and manually converts inline <code>style</code>s from CSS syntax to JSON syntax.</td>
    </tr>
    <tr>
      <th>Algorithm to update DOM</th>
      <td>Precise data-binding, which is faster than virtual DOM</td>
      <td>Virtual DOM differentiation, which requires manually managed <code>key</code> attributes for complicated DOM.</td>
    </tr>
    <tr>
      <th>Lifecycle management for data-binding expressions</th>
      <td>Automatically</td>
      <td>N/A</td>
    </tr>
    <tr>
      <th>Statically type checking</th>
      <td>Yes, even for HTML tags and attribues</td>
      <td>No</td>
    </tr>
    <tr>
      <th>Learning curve</th>
      <td>Always easy</td>
      <td>Easy to start. Requires much more efforts to understand its corner cases.</td>
    </tr>
  </tbody>
</table>

See [Design](#design) section for more information.

## Getting started

We will build an Binding.scala web page during the following steps.

### Step 0: Setup a Sbt Scala.js project

See http://www.scala-js.org/tutorial/basic/ for information about how to setup such a project.

### Step 1: Add html.scala dependencies into your `build.sbt`:

``` sbt
// Enable macro annotations by setting scalac flags for Scala 2.13
scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(3L)) {
    Nil
  } if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Seq("-Ymacro-annotations")
  } else {
    Nil
  }
}

// Enable macro annotations by adding compiler plugins for Scala 2.12
libraryDependencies ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  }
}

libraryDependencies += "com.yang-bo" %%% "html" % "latest.release"
```

### Step 2: Create a `data` field, which contains some `Var` and `Vars` as data source for your data-binding expressions

``` scala
case class Contact(name: Var[String], email: Var[String])

val data = Vars.empty[Contact]
```

A `Var` represents a bindable variable,
which also implements `Binding` trait,
hence a `Var` can be seen as a binding expression as well.
If another expression depends on a `Var`, the value of the expression changes whenever value of the `Var` changes.

A `Vars` represents a sequence of bindable variables,
which also implements `BindingSeq` trait,
hence a `Vars` can be seen as a binding expression of a sequence as well.
If another comprehension expression depends on a `Vars`,
the value of the expression changes whenever value of the `Vars` changes.

### Step 3: Create a `@html` method that contains data-binding expressions

``` scala
// For Scala 3
def table: Binding[HTMLTableElement] = {
  html"""<table border="1" cellPadding="5">
    <thead>
      <tr>
        <th>Name</th>
        <th>E-mail</th>
      </tr>
    </thead>
    <tbody>
      ${
        for (contact <- data) yield {
          html"""<tr>
            <td>
              ${contact.name.bind}
            </td>
            <td>
              ${contact.email.bind}
            </td>
          </tr>"""
        }
      }
    </tbody>
  </table>"""
}
```
``` scala
// For Scala 2
@html
def table: Binding[HTMLTableElement] = {
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
              {contact.name.bind}
            </td>
            <td>
              {contact.email.bind}
            </td>
          </tr>
        }
      }
    </tbody>
  </table>
}
```

`html"""..."""` interpolation in Scala 3 (or @html annotated methods in Scala 3) represents an reactive XHTML template, which supports HTML literal.
The type of HTML interpolation/literal is a specific subtype of `com.thoughtworks.binding.Binding[org.scalajs.dom.Node]` or `com.thoughtworks.binding.Binding.BindingSeq[org.scalajs.dom.Node]`,
instead of `scala.xml.Node` or `scala.xml.NodeSeq`.
So we could have `def node: Binding[HTMLBRElement] = html"""<br/>"""`
and `def node: BindingSeq[HTMLBRElement] = html"""<br/><br/>"""`.

A HTML interpolation/literal method is composed with other data-binding expressions in two ways:

 1. You could use `bind` method in an interpolation to get the value of another `Binding`.
 2. You could use `for` / `yield` expression in a `@html` method to map a `BindingSeq` to another.

You can nest `Node` or `BindingSeq[Node]` in other HTML element literals via `{ ... }` interpolation syntax.

### Step 4: Render the data-binding expressions to DOM in the `main` method

``` scala
@JSExport
def main(): Unit = {
  html.render(document.body, table)
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

Now you will see a table that just contains a header, because `data` is empty at the moment.

### Step 6: Add some `<button>` to fill `data` for the table

``` scala
def table: BindingSeq[Node] = {
  html"""<div>
    <button
      onclick=${ event: Event =>
        data.value += Contact(Var("Yang Bo"), Var("yang.bo@rea-group.com"))
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
      ${
        for (contact <- data) yield {
          <tr>
            <td>
              ${contact.name.bind}
            </td>
            <td>
              ${contact.email.bind}
            </td>
            <td>
              <button
                onclick=${ event: Event =>
                  contact.name.value = "Modified Name"
                }
              >
                Modify the name
              </button>
            </td>
          </tr>
        }
      }
    </tbody>
  </table>"""
}
```

When you click the "Add a contact" button, it appends a new Contact into `data`,
then, Binding.scala knows the relationship between DOM and `data`,
so it decides to append a new `<tr>` corresponding to the newly appended Contact.

And when you click the "Modify the name", the `name` field on `contact` changes,
then, Binding.scala decides to change the content of the corresponding `tr` to new value of `name` field.


## Design

### Precise data-binding

ReactJS requires users to provide a `render` function for each component.
The `render` function should map `props` and `state` to a ReactJS's virtual DOM,
then ReactJS framework creates a DOM with the same structure as the virtual DOM.

When `state` changes, ReactJS framework invokes `render` function to get a new virtual DOM.
Unfortunately, ReactJS framework does not precisely know what the `state` changing is.
ReactJS framework has to compare the new virtual DOM and the original virtual DOM,
and guess the changeset between the two virtual DOM,
then apply the guessed changeset to the real DOM as well.

For example, after you prepend a table row `<tr>` into an existing `<tbody>` in a `<table>`,
ReactJS may think you also changed the content of every existing `<tr>` of the `<tbody>`.

The reason for this is that the `render` function for ReactJS does not describe the relationship between `state` and DOM.
Instead, it describes the process to create a virtual DOM.
As a result, the `render` function does not provide any information about the purpose of the `state` changing,
although a data-binding framework should need the information.

Unlike ReactJS, a Binding.scala `@html` method is NOT a regular function.
It is a template that describes the relationship between data source and the DOM.
When part of the data source changes, Binding.scala knows about the exact corresponding partial DOM affected by the change,
thus only re-evaluating that part of the `@html` method to reflect the change in the DOM.

With the help of the ability of precise data-binding provided by Binding.scala,
you can get rid of concepts for hinting ReactJS's guessing algorithm,
like `key` attribute, `shouldComponentUpdate` method, `componentDidUpdate` method or `componentWillUpdate` method.

### Composability

The smallest composable unit in ReactJS is a component.
It is fair to say that a React component is lighter than an AngularJS controller,
while Binding.scala is better than that.

The smallest composable unit in Binding.scala is a `@html` method.
Every `@html` method is able to compose other `@html` methods via `.bind`.

``` scala
case class Contact(name: Var[String], email: Var[String])

def bindingButton(contact: Contact): Binding[Button] = {
  html"""<button
    onclick=${ event: Event =>
      contact.name.value = "Modified Name"
    }
  >
   Modify the name
  </button>"""
}

def bindingTr(contact: Contact): Binding[TableRow] = {
  html"""<tr>
    <td>${ contact.name.bind }</td>
    <td>${ contact.email.bind }</td>
    <td>${ bindingButton(contact).bind }</td>
  </tr>"""
}

def bindingTable(contacts: BindingSeq[Contact]): Binding[Table] = {
  html"""<table>
    <tbody>
      ${
        for (contact <- contacts) yield {
          bindingTr(contact).bind
        }
      }
    </tbody>
  </table>"""
}

@JSExport
def main(): Unit = {
  val data = Vars(Contact(Var("Yang Bo"), Var("yang.bo@rea-group.com")))
  dom.render(document.body, bindingTable(data))
}
```

You may find out this approach is much simpler than ReactJS, as:

 * Instead of passing `props` in ReactJS, you just simply provide parameters for  Binding.scala.
 * Instead of specifying `propTypes` in ReactJS, you just simply define the types of parameters in Binding.scala.
 * Instead of raising a run-time error when types of props do not match in ReactJS, you just check the types at compile-time.

### Lifecycle management for data-binding expressions

The ability of precise data-binding in Binding.scala requires listener registrations on the data source.
Other reactive frameworks that have the ability ask users manage the lifecycle of data-binding expressions.

For example, [MetaRx](https://github.com/MetaStack-pl/MetaRx/issues/45) provides a `dispose` method to unregister the listeners created when building data-binding expressions.
The users of MetaRx have the responsibility to call `dispose` method for every `map` and `flatMap` call after the expression changes,
otherwise MetaRx leaks memory. Unfortunately, manually `dispose`ing everything is too hard to be right for complicated binding expressions.

Another reactive web framework [Widok](https://github.com/widok/widok/issues/29) did not provide any mechanism to manage lifecycle of of data-binding expressions.
As a result, it simply always leaks memory.

In Binding.scala, unlike MetaRx or Widok, all data-binding expressions are pure functional, with no side-effects.
Binding.scala does not register any listeners when users create individual expressions,
thus users do not need to manually unregister listeners for a single expression like MetaRx.

Instead, Binding.scala creates all internal listeners together,
when the user calls `dom.render` or `Binding.watch` on the root expression.
Note that `dom.render` or `Binding.watch` manages listeners on all upstream expressions,
not only the direct listeners of the root expression.

In brief, Binding.scala separates functionality in two kinds:
 * User-defined `@html` methods, which produce pure functional expressions with no side-effects.
 * Calls to `dom.render` or `Binding.watch`, which manage all side-effects automatically.

### HTML literal and statically type checking

As you see, you can embed HTML literals in `@html` methods in Scala source files.
You can also embed Scala expressions in braces in content or attribute values of the HTML literal.

``` scala
def notificationBox(message: String): Binding[Div] = {
  html"""<div class="notification" title=${ s"Tooltip: $message" }>
    {
      message
    }
  </div>"""
}
```

Despite the similar syntax of HTML literal between Binding.scala and ReactJS,
Binding.scala creates real DOM instead of ReactJS's virtual DOM.

In the above example, `<div>...</div>` creates a DOM element with the type of `org.scalajs.dom.html.Div`.
Then, the magic `@html` lets the method wrap the result as a `Binding`.

You can even assign the `HTMLDivElement` to a local variable and invoke native DOM methods on the variable:

``` scala
def notificationBox(message: String): Binding[HTMLDivElement] = {
  val result: Binding.Stable[HTMLDivElement] = html"""<div class="notification" title=${ s"Tooltip: $message" }>
    ${
      message
    }
  </div>"""

  result.value.scrollIntoView()

  result
}
```

`scrollIntoView` method will be invoked when the `HTMLDivElement` is created.
If you invoke another method not defined in `HTMLDivElement`,
the Scala compiler will report a compile-time error instead of bringing the failure to run-time,
because Scala is a statically typed language and the Scala compiler understands the type of `Div`.

You may also notice `class` and `title`. They are DOM properties or HTML attributes on `Div`.
They are type-checked by Scala compiler as well.

For example, given the following `typo` method:

``` scala
def typo = {
  val myDiv = html"""<div typoProperty="xx">content</div>"""
  myDiv.value.typoMethod()
  myDiv
}
```

The Scala compiler will report errors like this:

```
typo.scala:23: typoProperty is neither a valid property nor a valid attribute for <DIV>
        val myDiv = html"""<div typoProperty="xx">content</div>"""
                            ^
typo.scala:24: value typoMethod is not a member of org.scalajs.dom.HTMLDivElement
        myDiv.value.typoMethod()
                    ^
```

With the help of the static type system, `@html` methods can be much more robust than ReactJS components.

You can find a complete list of supported properties and methods on [scaladoc of scalajs-dom](http://www.scala-js.org/api/scalajs-dom/0.8/org/scalajs/dom/raw/HTMLElement.html) or [MDN](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement)

## Showcases

 * [TodoMVC](http://todomvc.com/examples/binding-scala/): a project which offers the same Todo application implemented using MV* concepts in most of the popular JavaScript MV* frameworks of today.
 * [Granblue Raid Finder](https://github.com/walfie/gbf-raidfinder): a site for finding Granblue Fantasy raid tweets.
 * [Game of Life](https://github.com/zhanglongyang/game-of-life): Conway's Game of Life implemented with Binding.scala.
 * [playground-binding.scala
](https://github.com/ccamel/playground-binding.scala): Various DEMOs with scala, scalajs and binding.scala
 * [CITE Application](https://github.com/cite-architecture/CITE-App): A single-page browser application for exploring citable resources.
 * [hmt-reader](https://github.com/homermultitext/hmt-reader): A package of application and data for reading Homer Multitext textual data, in its current release.
 * [Full-Stack-Scala-Starter](https://github.com/Algomancer/Full-Stack-Scala-Starter): Play 2.5, ScalaJS, Binding.scala starter project.
 * [scala-adapters](https://pme123.github.io/scala-adapters/): A simple framework to implement your server jobs - providing a standard UI-client to monitor and test them. (Used in Production)
 * [Binding.scala-Google-Maps](https://github.com/pme123/Binding.scala-Google-Maps): A step-by-step tutorial to get you started with Binding.scala. 
 * [scala-adapters.g8](https://github.com/pme123/scala-adapters.g8): A Giter8 template for a full-stack Scala project that uses [scala-adapters](https://pme123.github.io/scala-adapters/).
 * [play-akka-telegrambot4s-incidents](https://github.com/pme123/play-akka-telegrambot4s-incidents): An incident management app - where you can send incidents with a chat bot.                                                                
 * [scala-adapters-images](https://github.com/pme123/scala-adapters-images): A demo project that uses [scala-adapters](https://pme123.github.io/scala-adapters/) and [play-akka-telegrambot4s](https://github.com/pme123/play-akka-telegrambot4s).
 * [jmh-view](https://github.com/tom91136/jmh-view): An embeddable JMH report viewer, which is used to to create [the report for the benchmarks of Scala parsers](https://tom91136.github.io/scala-parser-benchmarks/report.html).
 * [RL-Playground](https://github.com/basimkhajwal/RL-Playground): A web-based interactive reinforcement learning demonstration for games.
 * [Word Cloud Generator](https://github.com/emanresusername/word-cloud-generator): A browser extension to generate word cloud visualizations of web pages, text files, or other arbitrary text inputs.


(Feel free to add your project here)

## Modules

Binding.scala has an extremely tiny code base.
The source files are split into few libraries, one file per library.

### Core data-binding expressions (Binding.scala)

This module is available for both JVM and Scala.js. You could add it in your `build.sbt`.

``` sbt
// For JVM projects
libraryDependencies += "com.thoughtworks.binding" %% "binding" % "latest.release"
```

``` sbt
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "com.thoughtworks.binding" %%% "binding" % "latest.release"
```

### HTML DOM integration (html.scala)

This is the new HTML templating library based on [Name Based XML Literals](https://docs.scala-lang.org/sips/name-based-xml.html), the module is only available for Scala.js, and the Scala version must between 2.12 and 2.13. You could add it in your `build.sbt`.

``` sbt
// Enable macro annotations by setting scalac flags for Scala 2.13
scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Seq("-Ymacro-annotations")
  } else {
    Nil
  }
}

// Enable macro annotations by adding compiler plugins for Scala 2.12
libraryDependencies ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  }
}

// For Scala.js projects (Scala 2.12 - 2.13)
libraryDependencies += "com.yang-bo" %%% "html" % "latest.release"
```

See [html.scala](https://github.com/GlasslabGames/html.scala) for more information.

### Remote data-binding for `scala.concurrent.Future` (FutureBinding.scala)

This module is available for both JVM and Scala.js. You could add it in your `build.sbt`.

``` sbt
// For JVM projects
libraryDependencies += "com.thoughtworks.binding" %% "futurebinding" % "latest.release"
```

``` sbt
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "com.thoughtworks.binding" %%% "futurebinding" % "latest.release"
```

See [FutureBinding](https://github.com/Atry/FutureBinding.scala) for more information.

### Remote data-binding for ECMAScript 2015's `Promise` (JsPromiseBinding.scala)

This module is only available for Scala.js. You could add it in your `build.sbt`.

``` sbt
// For Scala.js projects
libraryDependencies += "com.thoughtworks.binding" %%% "jspromisebinding" % "latest.release"
```

See [FutureBinding](https://github.com/Atry/JsPromiseBinding.scala) for more information.

## Requirements

Due to collection API changes, Binding.scala 12.x only works on Scala 2.13, targeting JVM, Scala.js 0.6 and Scala.js 1.x.

For Scala 2.10, 2.11 and 2.12 on JVM or Scala.js 0.6, use [Binding.scala 11.x](https://github.com/ThoughtWorksInc/Binding.scala/tree/11.9.x) instead.

## Related projects

* [html.scala](https://github.com/Atry/html.scala) - HTML templating library built with Binding.scala
* [LatestEvent.scala](https://github.com/ThoughtWorksInc/LatestEvent.scala) - Event handling and URL routing for Binding.scala
* [FutureBinding.scala](https://www.javadoc.io/doc/com.thoughtworks.binding/futurebinding_3/latest/com/thoughtworks/binding/FutureBinding.html) - A wrapper that wraps a `scala.concurrent.Future` to a `Binding`.
* [JSPromiseBinding.scala](https://www.javadoc.io/doc/com.thoughtworks.binding/jspromisebinding_sjs1_3/latest/com/thoughtworks/binding/JSPromiseBinding.html) - A wrapper that wraps a JavaScript `Promise` to a `Binding`.
* [ReactToBindingHtml.scala](https://github.com/Atry/ReactToBindingHtml.scala) - React / Binding.scala / html.scala Interoperability
* [scalajs-all-in-one-template](https://github.com/Atry/scalajs-all-in-one-template) - All-in-One Scala.js Static Web Project Template (including Binding.scala)
* [Binding.scala • TodoMVC](http://todomvc.com/examples/binding-scala/) - An example todo app built with Binding.scala

## Other links

* [The API documentation](https://javadoc.io/page/com.thoughtworks.binding/binding_2.12/latest/com/thoughtworks/binding/index.html)
* [ScalaFiddle DEMOs](https://github.com/ThoughtWorksInc/Binding.scala/wiki/ScalaFiddle-DEMOs)
* [Binding.scala Questions on Stack Overflow](https://stackoverflow.com/questions/tagged/binding.scala)
* [本README的中文版](https://github.com/ThoughtWorksInc/Binding.scala/blob/10.0.x/README-zh.md)
