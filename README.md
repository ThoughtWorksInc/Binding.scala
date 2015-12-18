# Binding.scala

**Binding.scala** is a data-binding framework for [Scala](http://www.scala-lang.org/). It support [Scala.js](http://www.scala-js.org/), and enable you use native XML literal syntax to create reactive DOM nodes that be able to automatically change when the data source changes.

## Usage

### Step 1: add dependency configuration in your `build.sbt`:

For Scala JVM project::

``` scala
libraryDependencies += "au.com.realcommercial.binding" %% "core" % "1.0.1"
```

For Scala.js projects:

``` scala
libraryDependencies += "au.com.realcommercial.binding" %%% "core" % "1.0.1"

libraryDependencies += "au.com.realcommercial.binding" %%% "dom" % "1.0.1"
```

### Step 2: create `Var` and `Vars` as source of data-binding expression

``` scala
object Sample {

  case class Contact(name: Var[String], email: Var[String])
  def data = Vars.empty[Contact]

}
```

### Step 3: create `@dom` methods that contains data-binding expressions

``` scala
object Sample {

  case class Contact(name: Var[String], email: Var[String])
  def data = Vars.empty[Contact]

  @dom def table = <table>
    <thead>
      <tr><th>Name</th>
      <th>E-mail</th></tr>
    </thead>
    <tbody>
    {
      for (contact <- data) yield {
        <tr>
          <td>{contact.name.value}</td>
          <td>{contact.email.value}</td>
        </tr>
      }
    }
    </tbody>
  </table>

}
```

### Step 4: render the data-binding expressions to DOM

``` scala
object Sample {

  case class Contact(name: Var[String], email: Var[String])
  def data = Vars.empty[Contact]

  @dom def table = <table>
    <thead>
      <tr><th>Name</th>
      <th>E-mail</th></tr>
    </thead>
    <tbody>
    {
      for (contact <- data) yield {
        <tr>
          <td>{contact.name.value}</td>
          <td>{contact.email.value}</td>
        </tr>
      }
    }
    </tbody>
  </table>

  def main(): Unit = {
    dom.render(document.body, table)
  }

}
```
