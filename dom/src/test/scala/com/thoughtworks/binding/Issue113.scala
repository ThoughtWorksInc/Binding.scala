package com.thoughtworks.binding
import org.scalajs.dom.raw._
import org.scalajs.dom.{DragEvent, Event}

class Dashboard  {

  @dom
  def listBookmarks: Binding[HTMLDivElement] =
    <div>
      <ul>
        <li>
          <span ondragstart={e: DragEvent =>
}>
            <i>bookmark</i>
          </span> <button onclick={_: Event =>
          }>
          <i>delete</i>
        </button>
          <button onclick={_: Event =>

           }>
            <i>edit</i>
          </button>
        </li>
      </ul>

    </div>

  @dom
  def dialogUI(theId: String): Binding[HTMLElement] = <aside id={theId}>
    <div>
      <fieldset>

      </fieldset>

    </div>
  </aside>


//
//@dom
//def xx(theId: String): Binding[HTMLElement]  = {
// {
//  val element$macro$16 = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.aside.render;
//  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[_root_.com.thoughtworks.binding.Binding, _root_.scala.Unit](new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(element$macro$16, _root_.com.thoughtworks.binding.Binding.Constants(_root_.com.thoughtworks.binding.Binding.apply(_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq("""
//    """)), _root_.com.thoughtworks.binding.Binding.apply(_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq({
//  val element$macro$19 = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.div.render;
//  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[_root_.com.thoughtworks.binding.Binding, _root_.scala.Unit](new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(element$macro$19, _root_.com.thoughtworks.binding.Binding.Constants(_root_.com.thoughtworks.binding.Binding.apply(_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq("""
//      """)), _root_.com.thoughtworks.binding.Binding.apply(_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq({
//  val element$macro$20 = _root_.com.thoughtworks.binding.dom.Runtime.TagsAndTags2.fieldset.render;
//  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[_root_.com.thoughtworks.binding.Binding, _root_.scala.Unit](new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(element$macro$20, _root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq("""
//      """)));
//  element$macro$20
//})), _root_.com.thoughtworks.binding.Binding.apply(_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq("""
//    """))).flatMapBinding((_root_.scala.Predef.locally _))));
//  element$macro$19
//})), _root_.com.thoughtworks.binding.Binding.apply(_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq("""
//  """))).flatMapBinding((_root_.scala.Predef.locally _))));
//  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[_root_.com.thoughtworks.binding.Binding, _root_.scala.Unit](_root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]({
//    val newValue$macro$18 = theId;
//    @_root_.scala.inline def assignAttribute$macro$17() = if (_root_.com.thoughtworks.binding.dom.Runtime.notEqual(element$macro$16.id, newValue$macro$18))
//      element$macro$16.id = newValue$macro$18
//    else
//      ();
//    assignAttribute$macro$17()
//  }));
//  element$macro$16
//}
//
//}




}