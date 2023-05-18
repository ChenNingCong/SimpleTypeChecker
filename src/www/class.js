class SyntaxHead {
  kind;
  flag;
  constructor(kind, flag) {
    this.kind = kind;
    this.flag = flag;
  }
}
class ASTNode {
  head;
  parent;
  children;
  start;
  last;
  constructor(head, parent, start, last) {
    this.head = head;
    this.children = [];
    this.parent = parent;
    if (parent != null) {
      parent.children.push(this);
    }
    this.start = start;
    this.last = last;
  }
  selectAST(begin, end) {
    // not intersect
    if (begin > this.last || end < this.start) {
      return null;
    }
    if (this.start == begin && this.last == end) {
      return this;
    }
    // inclusion
    if (this.start <= begin && this.last >= end) {
      for (var i = 0; i < this.children.length; i++) {
        var rel = this.children[i].selectAST(begin, end);
        if (rel === null) {
          continue;
        } else {
          return rel;
        }
      }
      // inclusion, but not intersect with any element, a trivia
      return this;
    }
    // other case - partial insection
    return null;
  }
}

const onMouseHover = function () {
  start = this.getAttribute("start");
  if (start == null) {
    return;
  }
  last = this.getAttribute("last");
  if (last == null) {
    return;
  }
  var start = parseInt(start);
  var last = parseInt(last);
  //var rel = ast1.selectAST(start, last);
  //if (rel === null){
  //    return null;
  //}
  console.log(start);
  console.log(start);
  makeRequest(this, "locateNode " + start.toString() + " " + last.toString());
};
var codes = document.getElementsByClassName("code-text")[0].children;
for (var i = 0; i < codes.length; i++) {
  var elem = codes[i];
  elem.onmouseover = onMouseHover;
}

const TypeHintPopperElement = document.querySelector("#tooltip");
const TypeHintPopperSpanElement = document.querySelector("#tooltip-content");
function makeRequest(parent, str) {
  xhttp = new XMLHttpRequest();
  xhttp.open("POST", "http://127.0.0.1:3467");
  xhttp.onload = function () {
    console.log(this.responseText);
    TypeHintPopperSpanElement.innerHTML = this.responseText;
    Popper.createPopper(parent, TypeHintPopperElement, {placement : "bottom-end"});
  };
  xhttp.send(str);
}
