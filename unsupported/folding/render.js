// render_wiki_string(id, s)

var listLevel;

function render_wiki_string(id, s){
    var out, i, max;
    out = document.getElementById(id);
    render_wiki_str_in_node(out, s);
}

function render_wiki_str_in_node(out, s){
    // out.innerHTML = "XX";
    lines = s.split("\n");
    // max is the max index a string can have in lines
    max = lines.length-1;
    i = 0;
    listLevel = 0;
    while( i <= max){
	i = getnext(out, i, max, lines);
    };
}

// expands ...[[....|.....]] or
//         ...[[..........]]

function expand_links(s) {
    var start, stop, mid, linksep, link;
    while(true){
	start = s.indexOf("[[");
	if(start == -1)return s;
	stop = s.indexOf("]]", start+2);
	if(stop == -1)return s;
	var s1 = s.substring(0, start);
	var mid = s.substring(start+2, stop);
        var s2 = s.substring(stop+2, s.length);
	// mid might be in two segments
	linksep = mid.indexOf("|");
	if (linksep == -1) {
	    // there is only segment
	    link =  "<a href='one?arg=" + mid + "'>" + mid + "</a>";
	    s = s1 + link + s2;
	} else {
	    // two parts
	    var a = mid.substring(0, linksep);
	    var b = mid.substring(linksep+1, mid.length);
	    link = "<a href='two?arg=" + b + "'>" + a + "</a>"; 
	    // alert("a="+a+"b="+b+"link2="+link);
	    s = s1 + link + s2;
	}
    }
}

function expand_str(s) {
    s = expand_code(s);
    s = expand_inline(s, "//", "<i>", "</i>");
    s = expand_inline(s, "**", "<b>", "</b>");
    s = expand_links(s);
    s = expand_br(s);
    // do strike last
    s = expand_inline(s, "--", "<strike>", "</strike>");
    return(s);
}

// expand_br(s) -> s'  where ^ is replaces by <br>

function expand_br(s){
    var start,stop,mid;
    while(true){
	start = s.indexOf("^");
	if(start == -1)return s;
	var s1 = s.substring(0, start);
	var s2 = s.substring(start+1, s.length);
	s = s1 + "<br>" + s2;
    }
}

// expand_code replaces <<...>> by <<tt>...</tt>

function expand_code(s) {
    var start, stop, mid;
    while(true){
	start = s.indexOf("<<");
	if(start == -1)return s;
	stop = s.indexOf(">>", start+2);
	if(stop == -1)return s;
	var s1 = s.substring(0, start);
	var mid = s.substring(start+2, stop);
	var s2 = s.substring(stop+2, s.length);
	s = s1 + "<tt>" + mid + "</tt>" + s2;
    }
}

// expand the inline "term"
//   replace s1 term XXX term s2 by s1 wrapstart XXX wrapend s2

function expand_inline(s, term, wrapstart, wrapstop) {
    var start,stop,mid;
    while(true){
	start = s.indexOf(term);
	if(start == -1)return s;
	stop = s.indexOf(term, start+2);
	if(stop == -1)return s;
	var s1 = s.substring(0, start);
	var mid = s.substring(start+2, stop);
	var s2 = s.substring(stop+2, s.length);
	s = s1 + wrapstart + mid + wrapstop + s2;
    }
}

function getnext(out, i, max, lines){
    var s;
    s = lines[i];
    // out.innerHTML += "<br>getnext s="+s;
    if( s[0] == '>') {
	// alert("pre");
	// collect pre
	acc = s;
	while(i < max){
	    i++;
	    s = lines[i];
	    if (s[0] == '>'){
		acc += "\n" + s;
             } else {
                setListLevel(out, 0);
		out.innerHTML += "<pre>" + acc + "</pre>";
		return(i);
	    };
	};
        setListLevel(out, 0);
	out.innerHTML += "<pre>" + acc + "</pre>";
	return(max+1);
    } else if(s[0] == '!') {
        setListLevel(out, 0);
	collected_header(out, s);
	return(i+1);    
    } else if(s[0] == '*') {
	collected_list(out, s);
	return(i+1);    
    } else {
	// alert("para");
	acc = s;
	while(i < max){
	    i++;
	    s = lines[i];
	    // out.innerHTML += "<p>max="+max+" i="+i+" s="+s+"</p>";
	    if (s[0] == '>' || s[0] == "!" || s[0] == '*' || isallblank(s)) {
                setListLevel(out, 0);
		collected_para(out, acc);
		return(i);
	    } else {
		acc += " " + s;
	    }
	};
        setListLevel(out, 0);
	collected_para(out, acc);
	return(max+1);
    };
}

function setListLevel(out, n){
    if (listLevel == n) return;
    else if ( listLevel > n) {
	while(listLevel > n){
	    out.innerHTML += "</ul>";
	    listLevel--;
	};
    } else if (listLevel < n) {
	while(listLevel < n){
	    out.innerHTML += "<ul>";
	    listLevel++;
	}
    }
}


function collected_para(out, s){
    // now we have to expand ** ~~ etc....
    s = expand_str(s);
    out.innerHTML += "<p>" + s + "</p>";
}

function collected_header(out, s){
    var count, i;
    i=0;
    while(i < s.length && s[i] == '!')i++;
    // i = 1 for !
    // i = 2 for !!
    count = i;
    // advance i over any blanks
    while(s[i] == ' ' && i < s.length)i++;
    if (count == 1){
	out.innerHTML += "<h1>" + s.substring(i,s.length) + "</h1>";
    } else {
	out.innerHTML += "<h2>" + s.substring(i,s.length) + "</h2>";
    }
}

function collected_list(out, s){
    var count, i, s1;
    i=0;
    while(i < s.length && s[i] == '*')i++;
    // i = 1 for !
    // i = 2 for !!
    count = i;
    setListLevel(out, count);
    // advance i over any blanks
    while(s[i] == ' ' && i < s.length)i++;
    s1 = s.substring(i,s.length);
    out.innerHTML += "<li>" + expand_str(s1) + "</li>";
}


function isallblank(s) {
    var i;
    for(i = 0; i < s.length; i++){
	if(s[i] != ' ')return(false);
    };
    return(true);
}
