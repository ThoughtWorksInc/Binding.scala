# Binding.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Join the chat at https://gitter.im/ThoughtWorksInc/Binding.scala](https://badges.gitter.im/ThoughtWorksInc/Binding.scala.svg)](https://gitter.im/ThoughtWorksInc/Binding.scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/Binding.scala.svg)](https://travis-ci.org/ThoughtWorksInc/Binding.scala)
[![Maven Central (core funtionality)](https://img.shields.io/maven-central/v/com.thoughtworks.binding/binding_2.11.svg?label=maven-central%20%28Binding.scala%29)](https://maven-badges.herokuapp.com/maven-central/com.thoughtworks.binding/binding_2.11)
[![Maven Central (DOM integration)](https://img.shields.io/maven-central/v/com.thoughtworks.binding/dom_sjs0.6_2.11.svg?label=maven-central%20%28dom.scala%29)](https://maven-badges.herokuapp.com/maven-central/com.thoughtworks.binding/dom_sjs0.6_2.11)
[![Maven Central (remote data-binding for scala.concurrent.Future)](https://img.shields.io/maven-central/v/com.thoughtworks.binding/futurebinding_2.11.svg?label=maven-central%20%28FutureBinding.scala%29)](https://maven-badges.herokuapp.com/maven-central/com.thoughtworks.binding/futurebinding_2.11)
[![Maven Central (remote data-binding for ECMAScript 2015 Promise)](https://img.shields.io/maven-central/v/com.thoughtworks.binding/jspromisebinding_sjs0.6_2.11.svg?label=maven-central%20%28JsPromiseBinding.scala%29)](https://maven-badges.herokuapp.com/maven-central/com.thoughtworks.binding/jspromisebinding_sjs0.6_2.11)

**Binding.scala** 是一个用 [Scala](http://www.scala-lang.org/) 语言编写的数据绑定框架，可以在 JVM 和 [Scala.js](http://www.scala-js.org/) 上运行。

Binding.scala 可以用作 **[reactive](https://en.wikipedia.org/wiki/Reactive_programming) web freamework**。
它允许你使用原生 XHTML 语法去创建 reactive DOM 节点，这种 DOM 节点可以在数据源发生变化时自动地改变。

使用 Binding.scala 时可以参考 [Binding.scala • TodoMVC](http://todomvc.com/examples/binding-scala/) 或者[其他 DEMOs](https://thoughtworksinc.github.io/Binding.scala/)，他们中包含了许多常见功能的实现。

## 与其他 reactive web framework 对比

与其他 reactive web framework（例如 [ReactJS](https://facebook.github.io/react/)）对比，Binding.scala 有更多的特性以及更少的概念。

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
      <th>HTML 语法支持</th>
      <td>支持</td>
      <td>部分支持。Regular HTML 不会编译，除非开发者人为地将 <code>class</code> 属性和 <code>for</code> 属性改为 <code>className</code> 和 <code>htmlFor</code>，并且人为地将 <code>行内样式</code> 的语法从 CSS 语法改为 JSON 语法。</td>
    </tr>
    <tr>
      <th>DOM 更新算法</th>
      <td>精准的数据绑定，比虚拟 DOM 更快</td>
      <td>虚拟 DOM 之间存在差异，对于复杂的 DOM 你需要手动地管理 <code>key</code> 属性。</td>
    </tr>
    <tr>
      <th>数据绑定表达式的生命周期管理</th>
      <td>完全自动</td>
      <td>无</td>
    </tr>
    <tr>
      <th>静态类型检查</th>
      <td>支持，甚至是 HTML 标签和属性</td>
      <td>不支持</td>
    </tr>
    <tr>
      <th>学习曲线</th>
      <td>一直很简单</td>
      <td>容易上手，但在理解边界情况时需要投入大量精力。</td>
    </tr>
  </tbody>
</table>

更多详细信息，请查看[设计](#设计)。

## 开始使用

我们将在接下来的步骤里编写一个 Binding.scala 网页。

### 第 0 步：配置一个 Sbt Scala.js 项目

参考 http://www.scala-js.org/tutorial/basic/ 获取详细信息。

### 第 1 步：在你的 `build.sbt` 中添加 Binding.scala 依赖项

``` scala
libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

### 第 2 步：创建一个 `data` 域，其中包含一些 `Var` 和 `Vars` 作为你数据绑定表达式的数据源

``` scala
case class Contact(name: Var[String], email: Var[String])

val data = Vars.empty[Contact]
```

一个 `Var` 代表一个可绑定变量，
同时它也实现了 `Binding` 特性，
这就意味着一个 `Var` 也可以被视为一个是数据绑定表达式。
如果其他数据绑定表达式依赖与一个 `Var` ，那么该表达式的值将在这个 `Var` 的值改变时作出相应的改变。

一个 `Vars` 代表一个可绑定变量序列，
同时它也实现了 `BindingSeq` 特性，
这就意味着一个 `Vars` 也可以被视为一个序列的数据绑定表达式。
如果另一个数据绑定表达式依赖与一个 `Vars` ，
那么该表达式的值将在这个 `Vars` 的值改变时作出相应的改变。

### 第 3 步：创建一个含有数据绑定表达式的 `@dom` 方法

``` scala
@dom
def table: Binding[Table] = {
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

一个 `@dom` 方法代表一个数据绑定表达式。

其返回值类型永远被包装成 `com.thoughtworks.binding.Binding` 特性。
例如， `@dom def x: Binding[Int] = 1`， `@dom def message: Binding[String] = "content"`

`@dom` 方法支持 HTML 语法。
并不像通常 Scala 方法中的 XML 语法，
HTML 语法的类型是 `org.scalajs.dom.raw.Node` 或者 `com.thoughtworks.binding.BindingSeq[org.scalajs.dom.raw.Node]` 的子类型，
而不是 `scala.xml.Node` 或者 `scala.xml.NodeSeq`。
因此我们写出这样的代码，`@dom def node: Binding[org.scalajs.dom.raw.HTMLBRElement] = <br/>`
以及 `@dom def node: Binding[BindingSeq[org.scalajs.dom.raw.HTMLBRElement]] = <br/><br/>`。

由其他数据绑定表达式组成的 `@dom` 方法有两种编写方式：

 1. 你可以在 `@dom` 方法中使用 `bind` 方法来获取其他 `Binding` 的值。
 2. 你可以在 `@dom` 方法中使用 `for` 或者 `yield` 表达式将 `BindingSeq` 映射到其他的表达式上。

你可以通过使用 `{ ... }` 插入语法来在其他 HTML 元素中嵌入 `Node` 或者 `BindingSeq[Node]`。

### 第 4 步：在 `main` 方法中将数据绑定表达式渲染至 DOM

``` scala
@JSExport
def main(): Unit = {
  dom.render(document.body, table)
}
```

### 第 5 步： 在 HTML 页面中调用 `main` 方法

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

至此，你会看见一个只含有表头的空表格，这是因为 `data` 现在是空的。

### 第 6 步：添加一些 `<button>` 用于填充表格的 `data`

``` scala
@dom
def table: Binding[BindingSeq[Node]] = {
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
              {contact.name.bind}
            </td>
            <td>
              {contact.email.bind}
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

当你点击 "Add a contact" 按钮时， 它会在 `data` 中添加一个新的联系人，
于此同时，因为 Binding.scala 知道 DOM 与 `data` 之间的关系，
那么它就会添加一个新的、与新增的联系人信息对应的 `<tr>` 。

当你点击 "Modify the name" 按钮时，对应联系人的 `name` 将会发生改变，
这是因为 Binding.scala 将对应的 `tr` 中 `name` 的值改变了。

完整的例子请参考 https://github.com/ThoughtWorksInc/Binding.scala-sample 。


## 设计

### 精准的数据绑定

ReactJS 需要用户为每一个组件提供 `render` 函数。
`render` 函数需要将 `props` 和 `state` 映射至 ReactJS 的虚拟 DOM，
然后，ReactJS 框架会根据虚拟 DOM 的结构，创建一套结构相同的真实的 DOM。

当 `state` 改变时，ReactJS 框架会调用 `render` 函数去获取一个新的虚拟 DOM。
然而很不幸的是，ReactJS 并不能够确切地知道 `state` 改变了什么。
因此，ReactJS 不得不将新的虚拟 DOM 和原来的虚拟 DOM 进行比较，
并由此来猜测两个虚拟 DOM 之间的改变，最终应用到实际的 DOM 上。

举例来说，在你在 `<table>` 中的 `<tbody>` 的开头添加了一行 `<tr>`，
ReactJS 可能会认为你改变了 `<tbody>` 中所有的 `<tr>`，
并且在末尾添加了一行 `<tr>`。

原因是 ReactJS 中的 `render` 函数并不能描述 `state` 和 DOM 之间的关系。
相反，它表达的是创建虚拟 DOM 的过程。
也就是说，尽管作为数据绑定框架毫无疑问需要关于 `state` 改变的信息，但是 ReactJS 中的 `render` 函数却不能提供相关信息。

与 ReactJS 不同，Binding.scala 中的 `@dom` 方法并不是一个普通的函数。
它是一个描述数据源和 DOM 之间关系的模板。
当数据源发生部分改变时，Binding.scala 可以知道 DOM 中具体的哪些部分对应这些改变的数据。
因此，Binding.scala 只需要重新计算部分的 `@dom` 方法来获取部分的 DOM。

有着 Binding.scala 提供的精准的数据绑定能力的帮助，你可以摆脱不必要的、用于应对 ReactJS 的猜测算法的概念，
比如 `key` 属性，`shouldComponentUpdate` 方法，`componentDidUpdate` 方法以及 `componentWillUpdate`  方法。

### 模块性

在 ReactJS 中最小的组成单元是组件。

毫无疑问 React 组件要比 AngularJS 控制器更加轻量级，然而 Binding.scala 又比它更加优秀。

在 Binding.scala 中最小的组成单元仅仅是 `@dom` 方法。
每一个 `@dom` 方法有能力通过 `.bind` 组合其他的 `@dom` 方法。

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
    <td>{ contact.name.bind }</td>
    <td>{ contact.email.bind }</td>
    <td>{ bindingButton(contact).bind }</td>
  </tr>
}

@dom
def bindingTable(contacts: BindingSeq[Contact]): Binding[Table] = {
  <table>
    <tbody>
      {
        for (contact <- contacts) yield {
          bindingTr(contact).bind
        }
      }
    </tbody>
  </table>
}

@JSExport
def main(): Unit = {
  val data = Vars(Contact(Var("Yang Bo"), Var("yang.bo@rea-group.com")))
  dom.render(document.body, bindingTable(data))
}
```

你可以发现这种方法要比 ReactJS 简单很多，因为：

 * 你可以简单地为 Binding.scala 提供参数，而不用像在 ReactJS 中一样传递 `props`。
 * 在 Binding.scala 中，你可以简单地定义参数类型，而不用像在 ReactJS 中一样指定 `propTypes`。
 * 你可以在编译期进行类型检查，而不用像在 ReactJS 中一样只能在运行期间获得异常。

### 数据绑定表达式的生命周期管理

毫无疑问，Binding.scala 具有的精准数据绑定能力需要监听者对数据源注册。
其他的拥有这种能力的 reactive frameworks 需要用户管理数据绑定的生命周期。

例如，[MetaRx](https://github.com/MetaStack-pl/MetaRx/issues/45) 提供了 `dispose` 方法用于注销监听者。
在表达式改变后，每次调用 `map` 和 `flatMap` 方法，MetaRx 的用户都有义务去调用 `dispose` 方法，
否则 MetaRx 会有内存泄漏。然而非常不幸的是，对于复杂的绑定表达式，每次都需要手动地正确调用 `dispose` 实在是太困难了。

另一个 reactive web framework [Widok](https://github.com/widok/widok/issues/29) 没有提供任何机制用于管理数据绑定表达式的生命周期。
而这样的结果就是，Widok 一直在泄漏内存。

在 Binding.scala 中，与 MetaRx 以及 Widok 不同，所有的数据绑定表达式都是纯函数式的，不带有任何副作用。
Binding.scala 不会在用户创建表达式式注册任何的监听者，
因此用户并不需要像在 MetaRx 一样手动地注销监听者。

当用户在根表达式中调用 `dom.render` 或者 `Binding.watch` 时，Binding.scala 一同创建了所有的监听者，而不仅仅是根表达式的直接监听者。

简而言之，Binding.scala 将函数分为两类：
 * 用户定义的 `@dom` 方法，用于产生不带副作用的纯函数式的数据绑定表达式。
 * 调用 `dom.render` 以及 `Binding.watch` 的函数，自动地管理所有副作用。

### HTML 语法和静态类型检查

如你所见，你可以在 Scala 源码文件中的 `@dom` 方法里嵌入 HTML 语法。
你也可以在 HTML 标签里在 `{ ... }` 中或者在属性值中书写 Scala 代码。
``` scala
@dom
def notificationBox(message: String): Binding[Div] = {
  <div class="notification" title={ s"Tooltip: $message" }>
    {
      message
    }
  </div>
}
```

不论 HTML 语法在 Binding.scala 和 ReactJS 之间的相似性，
Binding.scala 创建真正的 DOM 而不是 ReactJS 中的虚拟 DOM。

在上述例子中，`<div>...</div>` 创建了一个类型为 `org.scalajs.dom.html.Div` 的 DOM 元素。
之后，神奇的 `@dom` 使得返回值被包装成 `Binding` 类型。

你甚至可以将 `Div` 赋值给局部变量并且在它上面调用原生 DOM 方法：

``` scala
@dom
def notificationBox(message: String): Binding[Div] = {
  val result: Div = <div class="notification" title={ s"Tooltip: $message" }>
    {
      message
    }
  </div>

  result.scrollIntoView()

  result
}
```

`scrollIntoView` 方法会在 `Div` 创建后被调用。
如果你在 `Div` 上调用了一个并没有定义的方法，
Scala 编译器会报告一个编译器错误而不是只有在运行时才会发生错误。
原因是 Scala 是静态类型语言而且 Scala 编译器可以理解 `Div` 类型。

你可能也注意到了 `class` 和 `title`。 他们是 DOM 属性或者 `Div` 上的 HTML 属性。
他们也会被 Scala 编译器进行类型检查。

例如，给出下面的 `typo` 方法：

``` scala
@dom
def typo = {
  val myDiv = <div typoProperty="xx">content</div>
  myDiv.typoMethod()
  myDiv
}
```

Scala 会报告出如下错误：

```
typo.scala:23: value typoProperty is not a member of org.scalajs.dom.html.Div
        val myDiv = <div typoProperty="xx">content</div>
                     ^
typo.scala:24: value typoMethod is not a member of org.scalajs.dom.html.Div
        myDiv.typoMethod()
              ^
```

有着静态类型系统的帮助， `@dom` 方法可以比 ReactJS 组件具有更充足的鲁棒性。

你可以在 [scaladoc of scalajs-dom](http://www.scala-js.org/api/scalajs-dom/0.8/org/scalajs/dom/raw/HTMLElement.html) 或者 [MDN](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement) 上找到一个完整的被支持的属性和方法的列表。

#### 自定义属性

如果你想要关闭属性的静态类型检查，在属性前添加 `data:` 前缀：

``` scala
@dom def myCustomDiv = {
  val myDiv = <div data:customAttributeName="attributeValue"></div>
  assert(myDiv.getAttribute("customAttributeName") == "attributeValue")
  myDiv
}
```

现在 Scala 编译器就不会报错了。

## 下载

Binding.scala 只有一个很小的代码基。
源代码被分为 4 个库，每个库一个文件。

### 数据绑定表达式核心 (Binding.scala)

这个模块在 JVM 和 Scala.js 上均可使用。你可以把它加入到你的 `build.sbt`。

``` scala
// For JVM projects
libraryDependencies += "com.thoughtworks.binding" %% "binding" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

``` scala
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "com.thoughtworks.binding" %%% "binding" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

### HTML DOM 集成 (dom.scala)

这个模块只可用于 Scala.js。你可以把它加入到你的 `build.sbt`。

``` scala
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

### 用于 `scala.concurrent.Future` 的远程数据绑定(FutureBinding.scala)

这个模块在 JVM 和 Scala.js 上均可使用。你可以把它加入到你的 `build.sbt`。

``` scala
// For JVM projects
libraryDependencies += "com.thoughtworks.binding" %% "futurebinding" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

``` scala
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "com.thoughtworks.binding" %%% "futurebinding" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

### 用于 ECMAScript 2015's `Promise` 的远程数据绑定(JsPromiseBinding.scala)

这个模块只可用于 Scala.js。你可以把它加入到你的 `build.sbt`。

``` scala
// For Scala.js projects, or JS/JVM cross projects
libraryDependencies += "com.thoughtworks.binding" %%% "jspromisebinding" % "latest.release"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

## 其他链接

* [The API documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/thoughtworks/binding/unidoc_2.11/9.0.2/unidoc_2.11-9.0.2-javadoc.jar/!/com/thoughtworks/binding/package.html)
* [Binding.scala • TodoMVC](http://todomvc.com/examples/binding-scala/)
* [Other live DEMOs](https://thoughtworksinc.github.io/Binding.scala/)
* [Frequently Asked Questions](https://github.com/ThoughtWorksInc/Binding.scala/wiki/FAQ)
