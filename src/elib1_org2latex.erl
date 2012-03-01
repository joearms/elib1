-module( elib1_org2latex).
-compile(export_all).
-import(lists, [reverse/1, reverse/2]).

batch([X]) ->
    io:format("formatting~p~n",[X]),
    start(atom_to_list(X)),
    init:stop().

start(In) ->
    Out = filename:rootname(In) ++ ".tex",
    L = elib1_misc:file2lines(In),
    {Mode, L1} = find_mode(L, []),
    %% elib1_misc:dump("test", L),
    O = [header(Mode),
	 %% next two lines are so that unicode quotes "66" .. "99" get converted"
	 "\\usepackage{ucs}\n"
	 "\\usepackage[utf8x]{inputenc}\n"
	 "\\begin{document}\n",
	 convert(L1, []),
	 "\n\\end{document}\n"],
    file:write_file(Out, [O]).

header(twoUp) ->
    "\\documentclass[twocolumn,a4paper,12pt]{book}\n"
	"\\usepackage[a4paper,landscape]{geometry}\n"
	"\\setlength{\\columnsep}{2.0cm}\n"
	"\\setlength{\\oddsidemargin}{0.0cm}\n"
	"\\setlength{\\evensidemargin}{0.0cm}\n"
	"\\setlength{\\textwidth}{10in}\n"
	"\\setlength{\\textheight}{6in}\n"
	"\\setlength{\\parskip}{12pt}\n"
	"\\setlength{\\columnseprule}{1pt}\n";
header(ebook) ->
    "\\documentclass[12pt]{book}\n"
	"%% lulu - 12 pt for on-screen reading\n"
	"\\setlength{\\marginparwidth}{0mm}\n"
	"\\setlength{\\textwidth}{4.3in}\n"
	"\\setlength{\\textheight}{7.0in}\n"
	"\\setlength{\\pdfpagewidth}{6in}\n"
	"\\setlength{\\pdfpageheight}{9in}\n"
	"\\setlength{\\hoffset}{-.25in}\n"
	"\\setlength{\\voffset}{-.3in}\n"
	"\\setlength{\\topmargin}{0in}\n"
	"\\setlength{\\oddsidemargin}{.25in}\n"
	"\\setlength{\\evensidemargin}{0mm}\n"
	"\\setlength{\\parskip}{12pt}\n".

find_mode(["\\layout{ebook}\n"|T], L) -> {ebook, reverse(L, T)};
find_mode(["\\layout{2up}\n"|T], L)   -> {twoUp, reverse(L, T)};
find_mode(["\\" ++ _|_]=T, L)         -> {twoUp, reverse(L, T)};
find_mode([H|T], L)                   -> find_mode(T, [H|L]);
find_mode([], L)                      -> {twoUp, reverse(L)}.
    
convert([[$<,Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2|_]|T], L) ->
    Line = "\\section*{" ++ [Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2] ++ "}\n",
    convert(T, [Line|L]);
convert(["* tolatex" ++ _|T], L) -> 
    %% convert to latex
    to_latex(T, L);
convert(["* hide" ++ _|T], L) -> 
    convert(T, L);
convert(["* " ++ Head|T], L) -> 
    Line = "\\section*{" ++ Head ++ "}",
    convert(T, [Line|L]);
convert([H|T], L) -> 
    convert(T, [H|L]);
convert([], L) ->
    reverse(L).

to_latex([[$<,_,_,_,_,$-,_,_,$-,_,_|_]|_]=X, L) ->
    convert(X, L);
to_latex(["* " ++ _|_]=X, L) -> 
    convert(X, L);
to_latex([H|T], L) ->
    to_latex(T, [elib1_misc:string2latex(H)|L]);
to_latex([], L) ->
    reverse(L).
