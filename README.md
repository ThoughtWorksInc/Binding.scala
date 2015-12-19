# Binding.scala

**Binding.scala** is a data-binding framework for [Scala](http://www.scala-lang.org/).
It support [Scala.js](http://www.scala-js.org/),
and enable you use native XML literal syntax to create reactive DOM nodes,
which are able to automatically change whenever the data source changes.

## Quick start for an web application

### Step 0: Setup a Sbt Scala.js project

See http://www.scala-js.org/tutorial/basic/ for information about how to setup such a project.

### Step 1: Add Binding.scala dependencies in your `build.sbt`:

``` scala
libraryDependencies += "au.com.realcommercial.binding" %%% "dom" % "1.0.1"
```

### Step 2: Create `Var` and `Vars` as source of data-binding expression

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

See https://git.realestate.com.au/rca/Binding.scala-demo for the complete example.