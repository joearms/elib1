var selected = 0;
var foldingMode = true;

var myClass = new Array();
myClass[1] = "zebra1";
myClass[2] = "zebra2";

function load() {
    var max, i;
    max = data.length;
    for(i=0;i<max;i++){
	data[i].edit = false;
	if(data[i].expanded != true) data[i].expanded = false;
    }
    selected = 0;
    render_rows();
}

function debug(){
    var out, i, max, x;
    out = document.getElementById("debug");
    out.innerHTML = "";
    out.innerHTML += "Debug";
    max = data.length;
    out.innerHTML += "<br>max="+max;
    out.innerHTML += "<br>selected="+selected;
    for(i=0; i < max; i++){
	out.innerHTML += "<br>";
	out.innerHTML += " expanded = "  + data[i].expanded;
	out.innerHTML += " edit = " + data[i].edit;
	out.innerHTML += " header = " + data[i].header;
    };
}

function render_rows() {
    if(foldingMode) render_folds();
}

function clear_static_display(){
  id = document.getElementById("statictab");
  id.innerHTML="";
}

function render_non_folding(){
    var i,id,max;
    id = document.getElementById("maintab");
    id.innerHTML = "";
    id = document.getElementById("statictab");
    max = data.length;
    for(i=0; i < max; i++){
	id.innerHTML += "<h1>" + data[i].header + "</h1>";
	render_wiki_str_in_node(id, data[i].content);
    };
}

function render_folds(){
    var node, node1, k,max,i,j,x,id,pos,node,oldmaxrows,color;
    max = data.length;
    // debug();
    pos = 0;
    // alert("max="+max);
    id = document.getElementById("maintab");
    id.innerHTML = "";
    // alert("stop");
    pos = 0;
    k = 1;
    for(i=0; i < max; i++){
	color = myClass[k];
	if(i == selected) color="selected";
	add_a_row(id, pos++, "header", data[i].header, i, color);
	if(data[i].expanded){
	    add_a_row(id, pos++, "data", "", i, "data");
	    node=id.rows[pos-1].cells[0];
	    // node.innerHTML = data[i].content;
	    render_wiki_str_in_node(node, data[i].content);
	};
	if(data[i].edit){
	    add_a_row(id, pos++, "data", "", i, "data");
	    node=id.rows[pos-1].cells[0];
	    // disable the window click event
	    document.body.setAttribute("onkeyup", "");
	    node1 = id.rows[pos-2].cells[0];
	    node.innerHTML = "<textarea cols=40 rows=1>" +
		data[i].header + "</textarea>" +
		"<textarea onkeyup='mynewclicker(event)' cols=40 rows=15>" +
		data[i].content + "</textarea>";
	    // set the focus in the editor
	    node.childNodes[0].focus();
	}
	k = 3 - k;
    };
}

function mynewclicker(event){
    var text = whichElement(event);
    var key = event.keyCode;
    alert("key="+key);
    var str = text.value;
    var row = text.parentNode.parentNode.rowIndex;
    var table = document.getElementById("maintab");
    var id = table.rows[row-1].cells[0];
    id.innerHTML="";
    render_wiki_str_in_node(id, str);
    if ( key == 27) {
	data[selected].content = str;
	data[selected].edit = false;
	// restore the origonal key handler for the body
	render_rows();
	document.body.setAttribute("onkeyup", "keypressed(event)");
    };
}

// add a row type is normal or expanded

function add_a_row(id, n, type, text, idx, class) {
    var row = id.insertRow(n);
    var y=row.insertCell(0);
    y.innerHTML = text;
    y.setAttribute("type", type);
    // y.setAttribute("onclick", "whichRow(event)");
    y.setAttribute("idx", idx);
    y.setAttribute("class", class);
    // color_rows(id);
}

function keypressed(event){
    var x, x1, x2, max, idx;
    var key = event.keyCode;
    id = document.getElementById("maintab");
    switch(key) {
    case 40:
	// down
	max = data.length;
	if (selected < max - 1)selected++;
	else selected=0;
	break;
    case 38:
	// move selection up
	if(selected > 0)selected--;
	else {
	    max = data.length;
	    selected = max - 1;
	};
	break;
     case 85:
	 // key 'u' move title up
	 if(selected > 0) {
	     x = data[selected];
	     data[selected] = data[selected-1];
	     data[selected-1] = x;
	     selected--;
	 };
	 break;
    case 68:
	// key 'd' move title down
	max = data.length;
	if(selected < max-1) {
	    x = data[selected];
	    data[selected] = data[selected+1];
	    data[selected+1] = x;
	    selected++;
	};
	break;
    case 69:
	// 'e'
	data[selected].edit  = ! data[selected].edit;
	if(data[selected].edit) data[selected].expanded = true;
	break;
    case 13:
	data[selected].expanded = ! data[selected].expanded;
	break;
    case 77:
	// 'm'
	foldingMode = ! foldingMode;
	if(foldingMode == true)clear_static_display();
	else render_non_folding();
	break;
    case 80:
	alert("pop");
	break;
    default:
	alert("keycode unrecognised =" + key);
	break;
    };
    render_rows();
}

function is_expanded(id, index) {
    return expanded[id.rows[index].cells[0].getAttribute("idx")];
}

function delete_a_row(id, n) {
    var x=document.getElementById(id).deleteRow(n);
    color_rows(id);
}

function text_a_row(id, n, c) {
    var x=document.getElementById(id).rows[n].cells
    x[0].innerHTML = c;
}

function style_a_row(id, n, c) {
    var x=document.getElementById(id).rows[n].cells
    x[0].setAttribute("class",c);
}

function set_an_attribute(id, n, attr, val) {
    var x = document.getElementById(id).rows[n].cells;
    x[0].setAttribute(attr, val);
}

function whichElement(e)
{
    var targ
    if (!e) var e = window.event
    if (e.target)
       targ = e.target
    else
       if (e.srcElement)
	  targ = e.srcElement
    if (targ.nodeType == 3) // defeat Safari bug
	targ = targ.parentNode
    return(targ);
}

function whichRow(e) {
    var td = whichElement(e);
    var tr = td.parentNode;
    index = + tr.rowIndex;
    var table = tr.parentNode.parentNode;
    var id = table.getAttribute("id");
    // alert("Click tab id=" + id + " index = " + index);

}
