class SyntaxHead{
    kind
    flag
    constructor(kind, flag) {
        this.kind = kind;
        this.flag = flag;
    }
}
class ASTNode {
    head
    parent
    children
    start
    last
    constructor(head, parent, start, last) {
        this.head = head;
        this.children = [];
        this.parent = parent;
        this.start = start;
        this.last = last;
    }
    selectAST(begin, end) {
        // not intersect
        if (begin > this.last || end < this.start){
            return null;
        }
        if (this.start == begin && this.last == end){
            return this;
        }
        // inclusion
        if (this.start <= begin && this.last >= end){
            for (var i = 0; i < this.children.length;i++){
                var rel = this.children[i].selectAST(begin, end);
                if (rel === null){
                    continue
                }
                else{
                    return rel
                }
            }
            // inclusion, but not intersect with any element, a trivia
            return this;
        }
        // other case - partial insection
        return null;
    }
}

const onMouseClick = function (){
    var start = parseInt(this.getAttribute("start"));
    var last = parseInt(this.getAttribute("last"));
    var rel = ast1.selectAST(start, last);
    if (rel === null){
        return null;
    }
    console.log(rel)
    console.log("selected")
}
var codes = document.getElementsByClassName("code-text")[0].children;
for (var i = 0; i < codes.length; i++){
    var elem = codes[i];
    elem.onclick = onMouseClick;

}

