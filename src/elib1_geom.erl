%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.

-module(elib1_geom).

-compile(export_all).

%% intersect(L1, L2) -> Bool.

intersect({line,{X1,Y1},{X2,Y2}},{line,{X3,Y3},{X4,Y4}}) ->
    Denom = (Y4-Y3)*(X2-X1) - (X4-X3)*(Y2-Y1),
    if 
	Denom == 0 ->
	    false;
	true ->
	    Ua = (X4-X3)*(Y1-Y3) - (Y4-Y3)*(X1-X3),
	    if 
		Ua >= 0, Ua =< 1 ->
		    Ub = (X2-X1)*(Y1-Y3) - (Y2-Y1)*(X1-X3),
		    if
			Ub >= 0, Ub =< 1 ->
			    true;
			true ->
			    false
		    end;
		true ->
		    false
	    end
    end.
%% http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
